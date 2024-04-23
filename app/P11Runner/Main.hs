{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, putMVar, readMVar)
import Control.Monad (forM_, when)
import Control.Monad.Free (Free)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, gets, modify)
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), eitherDecode, eitherDecodeFileStrict', eitherDecodeStrict, object, withObject, (.:))
import Data.Bifunctor (Bifunctor (first, second))
import Data.Fixed (Pico)
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq, ViewR (EmptyR, (:>)), viewr, (<|))
import Data.String (IsString)
import Data.Text (Text, intercalate, pack, strip, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (putStrLn)
import qualified Data.Text.Lazy as TL
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C (CInt, CLong)
import Foreign.C.String (CString, newCString)
import Foreign.Marshal (free, peekArray)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Marshal.Utils (new)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import P11Runner.DetectorTowerState (DetectorTowerConfig (..), DetectorTowerState, detectorTowerInMeasurementDistance, detectorTowerInSafeDistance, detectorTowerIsMoving, detectorTowerMoveToMeasurement, detectorTowerMoveToSafe, detectorTowerUpdate)
import P11Runner.Markdown (Markdown, markdownBold, markdownEmph, markdownPlain)
import P11Runner.Util (numberIsClose, numberIsCloseAbs)
import System.Environment (getArgs, getProgName)
import Tango.Common
  ( HaskellAttrWriteType (Read, ReadWrite),
    HaskellTangoDataType (HaskellDevBoolean, HaskellDevLong64, HaskellDevString, HaskellDevVoid),
    HaskellTangoDevState (Moving, On, Running, Unknown),
  )
import Tango.Server
  ( CommandCallback,
    DeviceInitCallback,
    DeviceInstancePtr,
    HaskellAttributeDefinition (..),
    HaskellCommandDefinition (..),
    createCommandCallback,
    createDeviceInitCallback,
    createGlobalFinalizer,
    tango_server_add_attribute_definition,
    tango_server_add_command_definition,
    tango_server_add_property,
    tango_server_init,
    tango_server_read_property,
    tango_server_set_state,
    tango_server_set_status,
    tango_server_start,
  )
import TangoHL
  ( AttributeName (AttributeName),
    CommandName (CommandName),
    DeviceProxyPtr,
    PropApplicative,
    PropertyName (PropertyName),
    ServerStatus (ServerStatus),
    TangoServerAttribute (TangoServerAttribute, tangoServerAttributeAccessor, tangoServerAttributeName),
    TangoServerAttributeAccessor (TangoServerAttributeAccessor),
    TangoServerAttributeTypes (TangoServerAttributeTypeString),
    TangoServerCommand (ServerCommandVoidVoid),
    TangoUrl,
    TypedProperty (TypedProperty),
    commandInOutVoid,
    gatherTypedPropertyNames,
    newDeviceProxy,
    readBoolAttribute,
    readDoubleAttribute,
    readLong64Attribute,
    readStateAttribute,
    readStringAttribute,
    readTypedProperty,
    readTypedTextProperty,
    readULong64Attribute,
    readUShortAttribute,
    resolveTypedProperties,
    tangoReadProperty,
    tangoServerInit,
    tangoServerStart,
    tangoUrlFromText,
    writeDoubleAttribute,
    writeInstanceState,
  )
import Text.Read (readMaybe)
import qualified UnliftIO
import UnliftIO.Foreign (peekCString, with, withArray, withCString)
import Prelude hiding (putStrLn)

data ChopperState
  = ChopperStateMovingIn
  | ChopperStateAccelerating
  | ChopperStateEnabled
  | ChopperStateDecelerating
  | ChopperStateMoving
  | ChopperStateDisabled
  deriving (Show)

data ColliConfig = ColliConfig
  { inY :: Double,
    inZ :: Double,
    outY :: Double,
    outZ :: Double,
    toleranceY :: Double,
    toleranceZ :: Double,
    movementTimeoutS :: NominalDiffTime
  }

data ColliConfigJson = ColliConfigJson
  { inY :: Double,
    inZ :: Double,
    outY :: Double,
    outZ :: Double
  }

instance FromJSON ColliConfigJson where
  parseJSON = withObject "ColliConfigJson" \v ->
    ColliConfigJson
      <$> v .: "in_y"
      <*> v .: "in_z"
      <*> v .: "out_y"
      <*> v .: "out_z"

readColliConfig :: (MonadIO m) => FilePath -> m (Either Text ColliConfigJson)
readColliConfig fp =
  liftIO $
    eitherDecodeFileStrict' fp >>= \case
      Left e -> pure $ Left (pack e)
      Right v -> pure (Right v)

colliConfigFromJson :: ColliConfigJson -> Double -> Double -> Double -> ColliConfig
colliConfigFromJson (ColliConfigJson {inY, inZ, outY, outZ}) movementTimeoutS toleranceY toleranceZ =
  ColliConfig
    { inY = inY,
      inZ = inZ,
      outY = outY,
      outZ = outZ,
      toleranceY = toleranceY,
      toleranceZ = toleranceZ,
      movementTimeoutS = secondsToNominalDiffTime (fromIntegral @Int @Pico $ round movementTimeoutS)
    }

data MeasuringState = MeasuringState
  { startTime :: !UTCTime,
    diodeTargetValueCount :: !Int,
    diodeValues :: ![Double],
    diodeCheckCompleted :: !Bool,
    chopperState :: !ChopperState,
    triggerSent :: !Bool
  }
  deriving (Show)

newtype PreparingForMeasurementData = PreparingForMeasurementData
  { collimatorEndTime :: Maybe UTCTime
  }
  deriving (Show)

newtype PreparingToOpenHutchData = PreparingToOpenHutchData
  { collimatorEndTime :: Maybe UTCTime
  }
  deriving (Show)

data RunnerState
  = RunnerStateUnknownMoving !Text
  | RunnerStateUnknownStatic !Text
  | RunnerStateStoppingChopper !ChopperState
  | RunnerStateMeasuring !MeasuringState
  | RunnerStatePreparingForMeasurement !PreparingForMeasurementData
  | RunnerStateReadyToMeasure
  | RunnerStatePreparingToOpenHutch !PreparingToOpenHutchData
  | RunnerStateReadyToOpenHutch
  deriving (Show)

data Proxies = Proxies
  { detectorTower :: !DeviceProxyPtr,
    chopper :: !DeviceProxyPtr,
    collimatorY :: !DeviceProxyPtr,
    collimatorZ :: !DeviceProxyPtr,
    eigerStream :: !DeviceProxyPtr,
    fastShutter :: !DeviceProxyPtr,
    eigerDetector :: !DeviceProxyPtr
  }

data DesiredCollimatorStatus = DesiredIn | DesiredOut deriving (Eq, Show)

data LogLevel = LogLevelInfo | LogLevelError | LogLevelDebug

instance ToJSON LogLevel where
  toJSON LogLevelInfo = toJSON ("info" :: Text)
  toJSON LogLevelError = toJSON ("error" :: Text)
  toJSON LogLevelDebug = toJSON ("debug" :: Text)

data Message = Message
  { messageText :: Text,
    messageLevel :: LogLevel,
    messageTime :: Int
  }

instance ToJSON Message where
  toJSON m = object ["text" .= m.messageText, "level" .= m.messageLevel, "when" .= m.messageTime]

data DeviceData = DeviceData
  { proxies :: Proxies,
    detectorTowerConfig :: DetectorTowerConfig,
    desiredCollimatorStatus :: DesiredCollimatorStatus,
    currentRunnerState :: RunnerState,
    currentDetectorTowerState :: DetectorTowerState,
    colliConfig :: ColliConfig,
    useChopper :: Bool,
    exposureTimeMs :: Double,
    numberOfImages :: Int,
    msgTrace :: Seq Message
  }

data P11RunnerState = P11RunnerState
  { runnerVar :: MVar DeviceData,
    runnerInstance :: DeviceInstancePtr
  }

type P11RunnerMonad = StateT P11RunnerState IO

updateDeviceData :: (DeviceData -> DeviceData) -> P11RunnerMonad ()
updateDeviceData f = do
  runnerVar' <- gets runnerVar
  liftIO (modifyMVar_ runnerVar' (pure . f))

runnerStateToTangoState :: RunnerState -> HaskellTangoDevState
runnerStateToTangoState (RunnerStateUnknownMoving _) = Moving
runnerStateToTangoState (RunnerStateUnknownStatic _) = On
runnerStateToTangoState (RunnerStateStoppingChopper _) = Moving
runnerStateToTangoState (RunnerStateMeasuring _) = Moving
runnerStateToTangoState (RunnerStatePreparingForMeasurement _) = Moving
runnerStateToTangoState RunnerStateReadyToMeasure = On
runnerStateToTangoState (RunnerStatePreparingToOpenHutch _) = Moving
runnerStateToTangoState RunnerStateReadyToOpenHutch = On

updateRunnerState :: RunnerState -> P11RunnerMonad ()
updateRunnerState newState = do
  updateDeviceData (\dd -> dd {currentRunnerState = newState})
  runnerVar' <- gets runnerVar
  runnerState <- liftIO $ readMVar runnerVar'
  instance' <- gets runnerInstance
  writeInstanceState instance' (runnerStateToTangoState runnerState.currentRunnerState)

readDeviceData :: P11RunnerMonad DeviceData
readDeviceData = do
  runnerVar' <- gets runnerVar
  liftIO (readMVar runnerVar')

readVar :: P11RunnerMonad (MVar DeviceData)
readVar = gets runnerVar

logToConsole :: Text -> P11RunnerMonad ()
logToConsole = liftIO . putStrLn

updatePreparingForMeasurement :: PreparingForMeasurementData -> RunnerState -> P11RunnerMonad ()
updatePreparingForMeasurement (PreparingForMeasurementData {collimatorEndTime}) calculatedState = do
  now <- liftIO getCurrentTime
  if maybe False (now <) collimatorEndTime
    then logToConsole "collimator timeout not expired yet, waiting"
    else case calculatedState of
      RunnerStateReadyToMeasure -> do
        appendMsg LogLevelInfo "colli timeout expired, ready to measure"
        updateRunnerState RunnerStateReadyToMeasure
      RunnerStateUnknownMoving reason -> do
        logToConsole $ "after colli timeout, we are still moving, specifically: " <> reason
      RunnerStateUnknownStatic reason -> do
        logToConsole $
          "timeout for preparing to for measurement expired, but we determined a static state (reason " <> reason <> "), this is okay if there's a slight timing mismatch"
      otherState -> do
        logToConsole "timeout for preparing to for measurement expired, but we determined a weird state state, switching to that state"
        updateRunnerState otherState

updatePreparingToOpenHutch :: PreparingToOpenHutchData -> RunnerState -> P11RunnerMonad ()
updatePreparingToOpenHutch (PreparingToOpenHutchData {collimatorEndTime}) calculatedState = do
  now <- liftIO getCurrentTime
  if maybe False (now <) collimatorEndTime
    then logToConsole "collimator timeout not expired yet, waiting"
    else case calculatedState of
      RunnerStateReadyToOpenHutch -> do
        appendMsg LogLevelInfo "colli timeout expired, ready to open hutch"
        updateRunnerState RunnerStateReadyToOpenHutch
      RunnerStateUnknownMoving reason -> do
        logToConsole $ "after colli timeout, we are still moving, specifically: " <> reason
      RunnerStateUnknownStatic reason -> do
        logToConsole $
          "timeout for preparing to for measurement expired, but we determined a static state (reason " <> reason <> "), this is okay if there's a slight timing mismatch"
      otherState -> do
        logToConsole "timeout for preparing to for measurement expired, but we determined a weird state state, switching to that state"
        updateRunnerState otherState

commandUpdate :: P11RunnerMonad ()
commandUpdate = do
  currentData <- readDeviceData
  newState <- liftIO $ calculateState currentData
  liftIO $ putStrLn $ "calculated state: " <> pack (show newState)
  liftIO $ putStrLn $ "current state: " <> pack (show currentData.currentRunnerState)
  case currentData.currentRunnerState of
    RunnerStatePreparingForMeasurement pfmData ->
      updatePreparingForMeasurement pfmData newState
    RunnerStatePreparingToOpenHutch pfmData ->
      updatePreparingToOpenHutch pfmData newState
    RunnerStateReadyToMeasure -> do
      runnerVar' <- gets runnerVar
      runnerState <- liftIO $ readMVar runnerVar'
      instance' <- gets runnerInstance
      writeInstanceState instance' (runnerStateToTangoState runnerState.currentRunnerState)
    RunnerStateUnknownMoving reason -> do
      liftIO $ putStrLn $ "in moving state because of " <> reason
      updateRunnerState newState
    _ -> pure ()

-- modifyMVar_ data' (\existing -> pure existing {currentRunnerState = newState})

utcTimeToMillis :: UTCTime -> Int
utcTimeToMillis t = round (utcTimeToPOSIXSeconds t / 1000)

msgTraceMaximum :: Int
msgTraceMaximum = 100

appendSeqLimited :: Int -> a -> Seq a -> Seq a
appendSeqLimited size value prior =
  if length prior < size
    then value <| prior
    else case viewr prior of
      EmptyR -> mempty
      allButRightmost :> _ -> value <| allButRightmost

appendMsgVar :: MVar DeviceData -> LogLevel -> Text -> IO ()
appendMsgVar data' logLevel' text' = do
  now <- getCurrentTime
  modifyMVar_ data' \oldData ->
    pure
      oldData
        { msgTrace =
            appendSeqLimited
              msgTraceMaximum
              ( Message
                  { messageText = text',
                    messageLevel = logLevel',
                    messageTime = utcTimeToMillis now
                  }
              )
              oldData.msgTrace
        }

appendMsg :: LogLevel -> Text -> P11RunnerMonad ()
appendMsg logLevel' text' = do
  var <- readVar
  liftIO (appendMsgVar var logLevel' text')

closeFastShutter :: P11RunnerMonad ()
closeFastShutter = do
  currentData <- readDeviceData
  appendMsg LogLevelInfo (markdownPlain "closing " <> markdownBold "fast shutter")
  liftIO $ commandInOutVoid currentData.proxies.fastShutter "PSOcontrolOff"

abortAcquisition :: P11RunnerMonad ()
abortAcquisition = do
  currentData <- readDeviceData
  appendMsg LogLevelInfo (markdownPlain "aborting acquisition")
  liftIO $ commandInOutVoid currentData.proxies.eigerDetector "Abort"
  liftIO $ commandInOutVoid currentData.proxies.eigerDetector "Disarm"

moveColliTo :: DesiredCollimatorStatus -> P11RunnerMonad ()
moveColliTo desired = do
  currentData <- readDeviceData
  let positionY = if desired == DesiredIn then currentData.colliConfig.inY else currentData.colliConfig.outY
      positionZ = if desired == DesiredIn then currentData.colliConfig.inZ else currentData.colliConfig.outZ
  appendMsg LogLevelInfo ("moving " <> markdownBold "collimator" <> ", this might take a while")
  liftIO $ writeDoubleAttribute currentData.proxies.collimatorY "Position" positionY
  liftIO $ writeDoubleAttribute currentData.proxies.collimatorZ "Position" positionZ

moveColliToDesired :: P11RunnerMonad ()
moveColliToDesired = do
  currentData <- readDeviceData
  moveColliTo currentData.desiredCollimatorStatus

raiseShield :: P11RunnerMonad ()
raiseShield = do
  currentData <- readDeviceData
  appendMsg LogLevelInfo (markdownPlain "raising " <> markdownBold "shield")
  liftIO $ commandInOutVoid currentData.proxies.detectorTower "ShieldUp"

shieldIsDown :: P11RunnerMonad Bool
shieldIsDown = do
  currentData <- readDeviceData
  liftIO $ readBoolAttribute currentData.proxies.detectorTower "ShieldIsDown"

fastShutterIsOpen :: P11RunnerMonad Bool
fastShutterIsOpen = do
  currentData <- readDeviceData
  liftIO $ readFastShutterOpen currentData.proxies.fastShutter

commandPrepareForMeasurement :: P11RunnerMonad ()
commandPrepareForMeasurement = do
  currentData <- readDeviceData
  case currentData.currentRunnerState of
    RunnerStatePreparingForMeasurement _ ->
      appendMsg LogLevelDebug (markdownPlain "already preparing for measurement, doing nothing")
    RunnerStateReadyToMeasure ->
      appendMsg LogLevelDebug (markdownPlain "already prepared for measurement, doing nothing")
    _otherState -> do
      appendMsg LogLevelInfo (markdownPlain "preparing for measurement")

      streamStatus <- liftIO $ readStateAttribute currentData.proxies.eigerStream

      case streamStatus of
        Running -> abortAcquisition
        other ->
          appendMsg
            LogLevelInfo
            (markdownBold "stream" <> " not " <> markdownEmph "busy" <> " but " <> packShow other <> ", not aborting")

      colliStatus <-
        checkCollimator
          currentData.proxies.collimatorY
          currentData.proxies.collimatorZ
          currentData.colliConfig
          currentData.desiredCollimatorStatus

      collimatorEndTime <- case colliStatus of
        AtLeastOneColliNotDesired _whichColliAxisIsNotDesired -> do
          appendMsg
            LogLevelInfo
            ("moving " <> markdownBold "collimator" <> " to position " <> packShow currentData.desiredCollimatorStatus)
          moveColliToDesired
          now <- liftIO getCurrentTime
          pure (Just (addUTCTime currentData.colliConfig.movementTimeoutS now))
        BothCollisInDesired -> pure Nothing

      (newState, log) <- detectorTowerMoveToMeasurement currentData.proxies.detectorTower currentData.detectorTowerConfig
      appendMsg LogLevelInfo log

      shieldIsDown' <- shieldIsDown

      if shieldIsDown'
        then raiseShield
        else appendMsg LogLevelDebug (markdownBold "shield" <> markdownPlain " already up")

      fastShutterIsOpen' <- fastShutterIsOpen

      if fastShutterIsOpen'
        then closeFastShutter
        else appendMsg LogLevelDebug (markdownBold "fast shutter" <> markdownPlain " already closed")

      updateRunnerState (RunnerStatePreparingForMeasurement (PreparingForMeasurementData collimatorEndTime))

commandPrepareToOpenHutch :: P11RunnerMonad ()
commandPrepareToOpenHutch = do
  currentData <- readDeviceData
  case currentData.currentRunnerState of
    RunnerStatePreparingToOpenHutch _ ->
      appendMsg LogLevelDebug (markdownPlain "already ready to open hutch, doing nothing")
    RunnerStateReadyToOpenHutch ->
      appendMsg LogLevelDebug (markdownPlain "already prepared for measurement, doing nothing")
    _otherState -> do
      appendMsg LogLevelInfo (markdownPlain "preparing to open hutch")

      shieldIsDown' <- shieldIsDown

      if shieldIsDown'
        then raiseShield
        else appendMsg LogLevelDebug (markdownBold "shield" <> markdownPlain " already up")

      fastShutterIsOpen' <- fastShutterIsOpen

      if fastShutterIsOpen'
        then closeFastShutter
        else appendMsg LogLevelDebug (markdownBold "fast shutter" <> markdownPlain " already closed")

      streamStatus <- liftIO $ readStateAttribute currentData.proxies.eigerStream

      case streamStatus of
        Running -> abortAcquisition
        other ->
          appendMsg
            LogLevelInfo
            (markdownBold "stream" <> " not " <> markdownEmph "busy" <> " but " <> packShow other <> ", not aborting")

      colliStatus <-
        checkCollimator
          currentData.proxies.collimatorY
          currentData.proxies.collimatorZ
          currentData.colliConfig
          DesiredOut

      collimatorEndTime <- case colliStatus of
        AtLeastOneColliNotDesired _whichColliAxisIsNotDesired -> do
          appendMsg LogLevelInfo ("moving " <> markdownBold "collimator" <> " out")
          moveColliTo DesiredOut
          now <- liftIO getCurrentTime
          pure (Just (addUTCTime currentData.colliConfig.movementTimeoutS now))
        BothCollisInDesired -> pure Nothing

      (newState, log) <- detectorTowerMoveToSafe currentData.proxies.detectorTower currentData.detectorTowerConfig
      appendMsg LogLevelInfo log

      updateRunnerState (RunnerStatePreparingToOpenHutch (PreparingToOpenHutchData collimatorEndTime))

flippedEvalState :: (Monad m) => s -> StateT s m a -> m a
flippedEvalState m initial = evalStateT initial m

finishUnknownStatic :: (Monad m) => Text -> StateT [Text] m RunnerState
finishUnknownStatic appendix = do
  messagesSoFar <- get
  pure $ RunnerStateUnknownStatic $ intercalate " and " messagesSoFar <> appendix

checkColliMotor :: forall m. (MonadIO m) => DeviceProxyPtr -> Double -> Double -> Double -> DesiredCollimatorStatus -> m Bool
checkColliMotor proxy inPos outPos tolerance desiredColliStatus = do
  pos <- liftIO $ readDoubleAttribute proxy "Position"
  case desiredColliStatus of
    DesiredIn ->
      pure (numberIsCloseAbs pos inPos tolerance)
    DesiredOut ->
      pure (numberIsCloseAbs pos outPos tolerance)

type ColliAxisDescription = Text

data CollimatorResult = BothCollisInDesired | AtLeastOneColliNotDesired (NE.NonEmpty ColliAxisDescription)

checkCollimator ::
  forall m.
  (MonadIO m) =>
  DeviceProxyPtr ->
  DeviceProxyPtr ->
  ColliConfig ->
  DesiredCollimatorStatus ->
  m CollimatorResult
checkCollimator collimatorY collimatorZ colliConfig desiredColliStatus = do
  yInDesired <- checkColliMotor collimatorY colliConfig.inY colliConfig.outY colliConfig.toleranceY desiredColliStatus
  zInDesired <- checkColliMotor collimatorZ colliConfig.inZ colliConfig.outZ colliConfig.toleranceZ desiredColliStatus

  if yInDesired
    then
      if zInDesired
        then pure BothCollisInDesired
        else pure (AtLeastOneColliNotDesired (NE.singleton "z"))
    else
      if zInDesired
        then pure (AtLeastOneColliNotDesired (NE.singleton "y"))
        else pure (AtLeastOneColliNotDesired ("z" NE.:| ["y"]))

checkChopper :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Bool -> m (Maybe Text)
checkChopper chopper useChopper = do
  chopperHolePosition <- readDoubleAttribute chopper "hole_position"
  if useChopper
    then case compareNumberIsCloseAbsMaybe "chopper position" chopperHolePosition 10.5 0.1 of
      Just chopperPositionError -> pure (Just chopperPositionError)
      Nothing -> pure Nothing
    else case compareNumberIsCloseAbsMaybe "chopper position" chopperHolePosition 0 0.1 of
      Just chopperPositionError -> pure (Just chopperPositionError)
      Nothing -> pure Nothing

readFastShutterOpen :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> m Bool
readFastShutterOpen fastShutterProxy = do
  fastShutterValue <- readUShortAttribute fastShutterProxy "PSOoutputStatus"
  pure (fastShutterValue /= 0)

compareValuesMaybe :: (Eq a, Show a) => Text -> a -> a -> Maybe Text
compareValuesMaybe description expected actual
  | expected == actual = Nothing
  | otherwise = Just (description <> " should be " <> packShow expected <> ", was " <> packShow actual)

compareNumberIsCloseAbsMaybe :: (Ord a, Fractional a, Show a) => Text -> a -> a -> a -> Maybe Text
compareNumberIsCloseAbsMaybe description expected actual tolerance
  | numberIsCloseAbs actual expected tolerance = Nothing
  | otherwise = Just (description <> " should be " <> packShow expected <> " (abs tolerance " <> packShow tolerance <> "), was " <> packShow actual)

isEigerConfigured :: (UnliftIO.MonadUnliftIO m) => Bool -> Double -> Int -> DeviceProxyPtr -> DeviceProxyPtr -> m (Maybe Text)
isEigerConfigured useChopper userExposureTimeMs numberOfImages chopperProxy eigerDetector = do
  frameTimeS <- readDoubleAttribute eigerDetector "FrameTime"
  countTimeS <- readDoubleAttribute eigerDetector "CountTime"
  nimages <- readULong64Attribute eigerDetector "Nimages"
  ntrigger <- readULong64Attribute eigerDetector "Ntrigger"
  triggerMode <- readStringAttribute eigerDetector "TriggerMode"
  chopperExposureTimeMs <- readDoubleAttribute chopperProxy "exposure_time"
  let -- Factor needed to fit the frame and count time in the duty cycle of the chopper
      -- (i.e. two trigger events). It is a bit less than 2 to avoid hitting the following trigger event.
      eigerExtsTriggerFrameTimeFactor = 1.95
      desiredFrameTimeMs =
        if useChopper
          then chopperExposureTimeMs * eigerExtsTriggerFrameTimeFactor
          else userExposureTimeMs
      desiredCountTimeMs = desiredFrameTimeMs
      desiredTriggerMode = if useChopper then "exts" else "ints"
      desiredNimages = if useChopper then 1 else numberOfImages
      desiredNtrigger = if useChopper then numberOfImages else 1

  pure $
    compareValuesMaybe "trigger mode" desiredTriggerMode triggerMode
      <|> compareValuesMaybe "nimages" desiredNimages nimages
      <|> compareValuesMaybe "ntrigger" desiredNtrigger ntrigger
      <|> compareNumberIsCloseAbsMaybe "frame time (ms)" desiredFrameTimeMs (frameTimeS * 1000.0) 0.1
      <|> compareNumberIsCloseAbsMaybe "count time (ms)" desiredCountTimeMs (countTimeS * 1000.0) 0.1

packShow :: (Show a) => a -> Text
packShow = pack . show

calculateState :: DeviceData -> IO RunnerState
calculateState
  currentData@( DeviceData
                  { proxies = Proxies {detectorTower, chopper, eigerStream, fastShutter, eigerDetector, collimatorY, collimatorZ},
                    useChopper,
                    exposureTimeMs,
                    numberOfImages,
                    detectorTowerConfig,
                    currentDetectorTowerState,
                    desiredCollimatorStatus,
                    colliConfig
                  }
                ) = do
    chopperState <- readStateAttribute chopper
    shieldIsDown' <- readBoolAttribute detectorTower "ShieldIsDown"
    shieldIsUp <- readBoolAttribute detectorTower "ShieldIsUp"
    collimatorYState <- readStateAttribute collimatorY
    collimatorZState <- readStateAttribute collimatorZ
    if detectorTowerIsMoving currentDetectorTowerState
      || collimatorYState == Moving
      || collimatorZState == Moving
      || chopperState /= On
      || shieldIsUp == shieldIsDown'
      then
        pure $
          RunnerStateUnknownMoving $
            "tower state (has to be On)) is "
              <> packShow currentDetectorTowerState
              <> ", collimator y/z movement states (have to be non-moving) are "
              <> packShow collimatorYState
              <> "/"
              <> packShow collimatorZState
              <> ", chopper state (has to be On) is "
              <> packShow chopperState
              <> " and shield down/up booleans are "
              <> packShow shieldIsDown'
              <> "/"
              <> packShow shieldIsUp
      else flippedEvalState ["nothing is moving"] do
        let addMessage message = modify (message :)
        if detectorTowerInMeasurementDistance currentDetectorTowerState
          then do
            addMessage "detector tower is in measuring distance"
            colliResult <- checkCollimator collimatorY collimatorZ colliConfig desiredCollimatorStatus
            case colliResult of
              AtLeastOneColliNotDesired (colli NE.:| []) -> finishUnknownStatic (", but the colli " <> colli <> " is not in desired position")
              AtLeastOneColliNotDesired _ -> finishUnknownStatic ", but both collimator axes are not in the desired position"
              BothCollisInDesired -> do
                addMessage "collimator is in desired position"
                if shieldIsUp
                  then do
                    addMessage "shield is up"
                    streamStatus <- liftIO $ readStateAttribute eigerStream
                    case streamStatus of
                      Running -> finishUnknownStatic ", but Eiger is busy (have you forgotten to interlock the hutch?)"
                      _ -> do
                        addMessage "Eiger is not busy"
                        fastShutterOpen <- liftIO $ readFastShutterOpen fastShutter
                        if fastShutterOpen
                          then finishUnknownStatic ", but the fast shutter is open"
                          else pure RunnerStateReadyToMeasure
                  else do
                    addMessage "shield is down"
                    streamStatus <- liftIO $ readStateAttribute eigerStream

                    if streamStatus /= Running
                      then -- This might happen during the diode measurement which
                      -- hasn't completed yet, I suppose? But then we're in
                      -- Measuring anyways, so this shouldn't disturb anyone.
                        finishUnknownStatic ", but the stream is not busy (wait a few seconds - if it persists, then we have a problem)"
                      else do
                        addMessage "stream is busy"
                        fastShutterOpen <- liftIO $ readFastShutterOpen fastShutter
                        if not fastShutterOpen
                          then finishUnknownStatic ", but the fast shutter is not open"
                          else do
                            eigerConfigurationCheck <-
                              liftIO $
                                isEigerConfigured
                                  useChopper
                                  exposureTimeMs
                                  numberOfImages
                                  chopper
                                  eigerDetector
                            case eigerConfigurationCheck of
                              Just eigerConfigError' -> finishUnknownStatic (", but " <> eigerConfigError')
                              Nothing -> do
                                addMessage "Eiger detector is configured correctly"
                                chopperError <- liftIO $ checkChopper chopper useChopper
                                case chopperError of
                                  Just chopperError' -> finishUnknownStatic (", but " <> chopperError')
                                  Nothing -> do
                                    addMessage (if useChopper then "Chopper is in" else "Chopper is out")
                                    currentTime <- liftIO getCurrentTime
                                    pure
                                      ( RunnerStateMeasuring
                                          ( MeasuringState
                                              { startTime = currentTime,
                                                diodeTargetValueCount = 0,
                                                diodeValues = [],
                                                diodeCheckCompleted = True,
                                                chopperState = if useChopper then ChopperStateEnabled else ChopperStateDisabled,
                                                triggerSent = True
                                              }
                                          )
                                      )
          else
            if not (detectorTowerInSafeDistance currentDetectorTowerState)
              then
                finishUnknownStatic
                  ( ", but the tower is neither in the safe distance nor in the measurement distance: "
                      <> packShow currentDetectorTowerState
                  )
              else do
                addMessage "tower is in safe distance"
                if shieldIsDown'
                  then finishUnknownStatic ", but the shield is down"
                  else do
                    addMessage "shield is up"
                    streamStatus <- liftIO $ readStateAttribute eigerStream
                    case streamStatus of
                      Running -> finishUnknownStatic ", but Eiger is busy"
                      _ -> do
                        addMessage "Eiger is not busy"
                        fastShutterOpen <- liftIO $ readFastShutterOpen fastShutter
                        if fastShutterOpen
                          then finishUnknownStatic ", but the fast shutter is open"
                          else pure RunnerStateReadyToMeasure

data P11RunnerProperties = P11RunnerProperties
  { propDetectorTowerIdentifier :: TangoUrl,
    propChopperIdentifier :: TangoUrl,
    propColliYIdentifier :: TangoUrl,
    propColliZIdentifier :: TangoUrl,
    propEigerStreamIdentifier :: TangoUrl,
    propFastShutterIdentifier :: TangoUrl,
    propDetectorIdentifier :: TangoUrl,
    propColliToleranceY :: Double,
    propColliToleranceZ :: Double,
    propTowerSafeDistanceMm :: Double,
    propTowerDistanceToleranceMm :: Double,
    propColliConfigFile :: FilePath,
    propColliMovementTimeoutS :: Double
  }

readMaybeText :: Text -> Either Text Double
readMaybeText x = case readMaybe (unpack x) of
  Nothing -> Left "not a valid number"
  Just v -> Right v

p11RunnerProperties :: PropApplicative P11RunnerProperties
p11RunnerProperties =
  P11RunnerProperties
    <$> readTypedProperty "detector_tower_identifier" tangoUrlFromText
    <*> readTypedProperty "chopper_identifier" tangoUrlFromText
    <*> readTypedProperty "colli_motor_identifier_y" tangoUrlFromText
    <*> readTypedProperty "colli_motor_identifier_z" tangoUrlFromText
    <*> readTypedProperty "eiger_stream_identifier" tangoUrlFromText
    <*> readTypedProperty "fast_shutter_identifier" tangoUrlFromText
    <*> readTypedProperty "detector_identifier" tangoUrlFromText
    <*> readTypedProperty "colli_position_tolerance_y" readMaybeText
    <*> readTypedProperty "colli_position_tolerance_z" readMaybeText
    <*> readTypedProperty "detector_tower_safe_distance_mm" readMaybeText
    <*> readTypedProperty "detector_distance_tolerance_mm" readMaybeText
    <*> readTypedProperty "colli_config" (Right . unpack)
    <*> readTypedProperty "collimator_movement_timeout_s" readMaybeText

initCallback :: MVar DeviceData -> DeviceInstancePtr -> IO ()
initCallback deviceData instance' = do
  putStrLn "in init callback, resolving properties"
  resolvedProps' <- resolveTypedProperties instance' p11RunnerProperties
  case resolvedProps' of
    Left e -> error $ "error resolving properties: " <> unpack e
    Right
      ( P11RunnerProperties
          { propDetectorTowerIdentifier,
            propChopperIdentifier,
            propColliYIdentifier,
            propColliZIdentifier,
            propEigerStreamIdentifier,
            propFastShutterIdentifier,
            propDetectorIdentifier,
            propColliConfigFile,
            propTowerSafeDistanceMm,
            propTowerDistanceToleranceMm,
            propColliToleranceY,
            propColliToleranceZ,
            propColliMovementTimeoutS
          }
        ) -> do
        proxies <-
          Proxies
            <$> newDeviceProxy propDetectorTowerIdentifier
            <*> newDeviceProxy propChopperIdentifier
            <*> newDeviceProxy propColliYIdentifier
            <*> newDeviceProxy propColliZIdentifier
            <*> newDeviceProxy propEigerStreamIdentifier
            <*> newDeviceProxy propFastShutterIdentifier
            <*> newDeviceProxy propDetectorIdentifier
        colliConfig <- readColliConfig propColliConfigFile
        case colliConfig of
          Left e -> error $ "invalid colli config file \"" <> propColliConfigFile <> "\": " <> unpack e
          Right colliConfig' -> do
            let detectorTowerConfig =
                  DetectorTowerConfig
                    { towerConfigSafeDistanceMm = propTowerSafeDistanceMm,
                      towerConfigMeasurementDistanceMm = 200.0,
                      towerConfigToleranceMm = propTowerDistanceToleranceMm
                    }
            towerState <- detectorTowerUpdate proxies.detectorTower detectorTowerConfig
            let deviceDataContent =
                  DeviceData
                    { proxies = proxies,
                      desiredCollimatorStatus = DesiredIn,
                      currentRunnerState = RunnerStateUnknownMoving "before first connect call",
                      detectorTowerConfig = detectorTowerConfig,
                      currentDetectorTowerState = towerState,
                      useChopper = False,
                      exposureTimeMs = 7.6,
                      numberOfImages = 1000,
                      colliConfig = colliConfigFromJson colliConfig' propColliMovementTimeoutS propColliToleranceY propColliToleranceZ,
                      msgTrace = mempty
                    }
            putMVar deviceData deviceDataContent
            putStrLn "initializing successful"

readTestAttribute :: DeviceInstancePtr -> IO Text
readTestAttribute _ = pure "lol"

main :: IO ()
main = do
  deviceData <- newEmptyMVar
  initedServerEither <-
    tangoServerInit
      (gatherTypedPropertyNames p11RunnerProperties)
      (ServerStatus "")
      Unknown
      [ TangoServerAttribute
          { tangoServerAttributeName = AttributeName "test_attribute",
            tangoServerAttributeAccessor = TangoServerAttributeTypeString (TangoServerAttributeAccessor readTestAttribute Nothing)
          }
      ]
      [ ServerCommandVoidVoid
          (CommandName "update")
          ( \instancePtr ->
              evalStateT
                commandUpdate
                ( P11RunnerState
                    { runnerVar = deviceData,
                      runnerInstance = instancePtr
                    }
                )
          ),
        ServerCommandVoidVoid
          (CommandName "prepare_for_measurement")
          ( \instancePtr ->
              evalStateT
                commandPrepareForMeasurement
                ( P11RunnerState
                    { runnerVar = deviceData,
                      runnerInstance = instancePtr
                    }
                )
          ),
        ServerCommandVoidVoid
          (CommandName "prepare_to_open_hutch")
          ( \instancePtr ->
              evalStateT
                commandPrepareToOpenHutch
                ( P11RunnerState
                    { runnerVar = deviceData,
                      runnerInstance = instancePtr
                    }
                )
          )
      ]
      (initCallback deviceData)
  case initedServerEither of
    Left e -> putStrLn ("error initializing: " <> e)
    Right initedServer -> tangoServerStart initedServer