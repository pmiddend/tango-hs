{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, putMVar, readMVar)
import Control.Exception (catch)
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
import Data.Time.Format.ISO8601 (iso8601Show)
import Foreign.C (CInt, CLong)
import Foreign.C.String (CString, newCString)
import Foreign.Marshal (free, peekArray)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Marshal.Utils (new)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import P11Runner.CollimatorState (ColliAxisValues (ColliAxisValues), ColliState, colliInit, colliMoveOut, colliMoveToDesired, isColliInDesired, isColliMoving, isColliOut)
import P11Runner.DetectorTowerState
  ( DetectorTowerConfig (..),
    DetectorTowerState (..),
    calculateDetectorTowerState,
    detectorTowerMeasurementDistance,
    detectorTowerMoveToMeasurement,
    detectorTowerMoveToSafe,
    updateDetectorTowerMeasurementDistance,
  )
import P11Runner.Markdown (Markdown, markdownBold, markdownEmph, markdownPlain)
import P11Runner.Util (logToConsole, numberIsClose, numberIsCloseAbs)
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
    HaskellDevFailed (HaskellDevFailed),
    PropApplicative,
    PropertyName (PropertyName),
    ServerStatus (ServerStatus),
    TangoException (TangoException),
    TangoServerAttribute (TangoServerAttribute, tangoServerAttributeAccessor, tangoServerAttributeName),
    TangoServerAttributeAccessor (TangoServerAttributeAccessor),
    TangoServerCommand (ServerCommandVoidVoid),
    TangoUrl,
    TypedProperty (TypedProperty),
    commandInOutVoid,
    devFailedDesc,
    devFailedOrigin,
    devFailedReason,
    devFailedSeverity,
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

data MeasuringState = MeasuringState
  { startTime :: !UTCTime,
    diodeTargetValueCount :: !Int,
    diodeValues :: ![Double],
    diodeCheckCompleted :: !Bool,
    chopperState :: !ChopperState,
    triggerSent :: !Bool
  }
  deriving (Show)

data RunnerState
  = RunnerStateUnknownMoving !Text
  | RunnerStateUnknownStatic !Text
  | RunnerStateStoppingChopper !ChopperState
  | RunnerStateMeasuring !MeasuringState
  | RunnerStatePreparingForMeasurement
  | RunnerStateReadyToMeasure
  | RunnerStatePreparingToOpenHutch
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

instance Show LogLevel where
  show LogLevelInfo = "INFO"
  show LogLevelError = "ERROR"
  show LogLevelDebug = "DEBUG"

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
    currentRunnerState :: RunnerState,
    currentColliState :: ColliState,
    currentDetectorTowerState :: DetectorTowerConfig,
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
runnerStateToTangoState RunnerStatePreparingForMeasurement = Moving
runnerStateToTangoState RunnerStateReadyToMeasure = On
runnerStateToTangoState RunnerStatePreparingToOpenHutch = Moving
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

updatePreparingForMeasurement :: RunnerState -> P11RunnerMonad ()
updatePreparingForMeasurement calculatedState = do
  case calculatedState of
    RunnerStateReadyToMeasure -> do
      appendMsg LogLevelInfo "colli timeout expired, ready to measure"
      updateRunnerState RunnerStateReadyToMeasure
    RunnerStateUnknownMoving reason -> do
      logToConsole $ "we are still moving, specifically: " <> reason
    RunnerStateUnknownStatic reason -> do
      logToConsole $
        "timeout for preparing to for measurement expired, but we determined a static state (reason " <> reason <> "), this is okay if there's a slight timing mismatch"
    otherState -> do
      logToConsole "timeout for preparing to for measurement expired, but we determined a weird state state, switching to that state"
      updateRunnerState otherState

updatePreparingToOpenHutch :: RunnerState -> P11RunnerMonad ()
updatePreparingToOpenHutch calculatedState =
  case calculatedState of
    RunnerStateReadyToOpenHutch -> do
      appendMsg LogLevelInfo "colli timeout expired, ready to open hutch"
      updateRunnerState RunnerStateReadyToOpenHutch
    RunnerStateUnknownMoving reason -> do
      logToConsole $ "we are still moving, specifically: " <> reason
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
  logToConsole $ "calculated state: " <> pack (show newState)
  logToConsole $ "current state: " <> pack (show currentData.currentRunnerState)
  case currentData.currentRunnerState of
    RunnerStatePreparingForMeasurement ->
      updatePreparingForMeasurement newState
    RunnerStatePreparingToOpenHutch ->
      updatePreparingToOpenHutch newState
    RunnerStateReadyToMeasure -> do
      runnerVar' <- gets runnerVar
      runnerState <- liftIO $ readMVar runnerVar'
      instance' <- gets runnerInstance
      writeInstanceState instance' (runnerStateToTangoState runnerState.currentRunnerState)
    RunnerStateUnknownMoving reason -> do
      logToConsole $ "in moving state because of " <> reason
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
  logToConsole (packShow logLevel' <> " " <> text')

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

updateColliState :: ColliState -> P11RunnerMonad ()
updateColliState newState = do
  updateDeviceData (\dd -> dd {currentColliState = newState})

commandStartRun :: P11RunnerMonad ()
commandStartRun = do
  logToConsole "command: start run"
  currentData <- readDeviceData
  case currentData.currentRunnerState of
    RunnerStateMeasuring _ -> do
      appendMsg LogLevelInfo (markdownPlain "starting run")
    RunnerStateReadyToMeasure -> do
      appendMsg LogLevelDebug (markdownPlain "already preparing for measurement, doing nothing")
    otherState -> do
      appendMsg LogLevelInfo (markdownPlain "cannot start a run, current state is " <> markdownEmph (packShow otherState))

commandPrepareForMeasurement :: P11RunnerMonad ()
commandPrepareForMeasurement = do
  logToConsole "command: prepare for measurement"
  currentData <- readDeviceData
  case currentData.currentRunnerState of
    RunnerStatePreparingForMeasurement ->
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

      (newColliState, colliLog) <- colliMoveToDesired currentData.currentColliState
      updateColliState newColliState
      appendMsg LogLevelInfo colliLog

      log' <- detectorTowerMoveToMeasurement currentData.currentDetectorTowerState
      appendMsg LogLevelInfo log'

      shieldIsDown' <- shieldIsDown

      if shieldIsDown'
        then raiseShield
        else appendMsg LogLevelDebug (markdownBold "shield" <> markdownPlain " already up")

      fastShutterIsOpen' <- fastShutterIsOpen

      if fastShutterIsOpen'
        then closeFastShutter
        else appendMsg LogLevelDebug (markdownBold "fast shutter" <> markdownPlain " already closed")

      updateRunnerState RunnerStatePreparingForMeasurement

commandPrepareToOpenHutch :: P11RunnerMonad ()
commandPrepareToOpenHutch = do
  logToConsole "command: prepare to open hutch"
  currentData <- readDeviceData
  case currentData.currentRunnerState of
    RunnerStatePreparingToOpenHutch ->
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

      (newColliState, colliLog) <- colliMoveOut currentData.currentColliState
      updateColliState newColliState
      appendMsg LogLevelInfo colliLog

      log' <- detectorTowerMoveToSafe currentData.currentDetectorTowerState
      appendMsg LogLevelInfo log'

      updateRunnerState RunnerStatePreparingToOpenHutch

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
  ( DeviceData
      { proxies = Proxies {detectorTower, chopper, eigerStream, fastShutter, eigerDetector},
        useChopper,
        exposureTimeMs,
        numberOfImages,
        currentDetectorTowerState,
        currentColliState
      }
    ) = do
    chopperState <- readStateAttribute chopper
    shieldIsDown' <- readBoolAttribute detectorTower "ShieldIsDown"
    shieldIsUp <- readBoolAttribute detectorTower "ShieldIsUp"
    colliMoving <- isColliMoving currentColliState
    latestTowerState <- calculateDetectorTowerState currentDetectorTowerState
    if latestTowerState == DetectorTowerMoving || colliMoving || chopperState /= On || shieldIsUp == shieldIsDown'
      then
        pure $
          RunnerStateUnknownMoving $
            "tower state (has to be On) is "
              <> packShow latestTowerState
              <> ", collimator movement state (have to be non-moving) is "
              <> (if colliMoving then "moving" else "non-moving")
              <> ", chopper state (has to be On) is "
              <> packShow chopperState
              <> " and shield down/up booleans are "
              <> packShow shieldIsDown'
              <> "/"
              <> packShow shieldIsUp
      else flippedEvalState ["nothing is moving"] do
        let addMessage message = modify (message :)
        if latestTowerState == DetectorTowerInMeasurement
          then do
            addMessage "detector tower is in measuring distance"
            colliInDesired <- isColliInDesired currentColliState
            if not colliInDesired
              then finishUnknownStatic ", but the colli is not in desired position"
              else do
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
            if latestTowerState /= DetectorTowerInSafe
              then
                finishUnknownStatic
                  ( ", but the tower is neither in the safe distance nor in the measurement distance: "
                      <> packShow latestTowerState
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
                          else do
                            colliIsOut <- isColliOut currentColliState
                            if not colliIsOut
                              then finishUnknownStatic ", but the colli is not out"
                              else pure RunnerStateReadyToOpenHutch

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
        colliState <-
          colliInit
            (pack propColliConfigFile)
            propColliMovementTimeoutS
            (ColliAxisValues propColliToleranceY propColliToleranceZ)
            (ColliAxisValues proxies.collimatorY proxies.collimatorZ)
        case colliState of
          Left e -> error $ "invalid colli config file \"" <> propColliConfigFile <> "\": " <> unpack e
          Right colliState' -> do
            let detectorTowerConfig =
                  DetectorTowerConfig
                    { towerConfigSafeDistanceMm = propTowerSafeDistanceMm,
                      towerConfigMeasurementDistanceMm = 200.0,
                      towerConfigToleranceMm = propTowerDistanceToleranceMm,
                      towerConfigProxy = proxies.detectorTower
                    }
            let deviceDataContent =
                  DeviceData
                    { proxies = proxies,
                      currentColliState = colliState',
                      currentRunnerState = RunnerStateUnknownMoving "before first connect call",
                      currentDetectorTowerState = detectorTowerConfig,
                      useChopper = False,
                      exposureTimeMs = 7.6,
                      numberOfImages = 1000,
                      msgTrace = mempty
                    }
            putMVar deviceData deviceDataContent
            putStrLn "initializing successful"

readDetectorTowerMeasurementDistance :: P11RunnerMonad Double
readDetectorTowerMeasurementDistance = do
  currentData <- readDeviceData
  pure (detectorTowerMeasurementDistance currentData.currentDetectorTowerState)

writeDetectorTowerMeasurementDistance :: Double -> P11RunnerMonad ()
writeDetectorTowerMeasurementDistance newDistance =
  updateDeviceData
    ( \dd ->
        dd
          { currentDetectorTowerState = updateDetectorTowerMeasurementDistance dd.currentDetectorTowerState newDistance
          }
    )

main :: IO ()
main = do
  deviceData <- newEmptyMVar
  let wrapVoidCommand commandName commandFn =
        ServerCommandVoidVoid
          (CommandName commandName)
          ( \instancePtr ->
              evalStateT
                commandFn
                ( P11RunnerState
                    { runnerVar = deviceData,
                      runnerInstance = instancePtr
                    }
                )
          )
  initedServerEither <-
    tangoServerInit
      (gatherTypedPropertyNames p11RunnerProperties)
      (ServerStatus "")
      Unknown
      [ TangoServerAttribute
          { tangoServerAttributeName = AttributeName "detector_tower_measurement_distance",
            tangoServerAttributeAccessor =
              TangoServerAttributeAccessor
                ( \instancePtr ->
                    evalStateT
                      readDetectorTowerMeasurementDistance
                      ( P11RunnerState
                          { runnerVar = deviceData,
                            runnerInstance = instancePtr
                          }
                      )
                )
                ( Just
                    ( \instancePtr newDistance ->
                        evalStateT
                          (writeDetectorTowerMeasurementDistance newDistance)
                          ( P11RunnerState
                              { runnerVar = deviceData,
                                runnerInstance = instancePtr
                              }
                          )
                    )
                )
          }
      ]
      [ wrapVoidCommand "update" commandUpdate,
        wrapVoidCommand "prepare_for_measurement" commandPrepareForMeasurement,
        wrapVoidCommand "start_run" commandStartRun,
        wrapVoidCommand "prepare_to_open_hutch" commandPrepareToOpenHutch
      ]
      (initCallback deviceData)
  putStrLn "starting server now"
  case initedServerEither of
    Left e -> putStrLn ("error initializing: " <> e)
    Right initedServer -> do
      tangoServerStart initedServer

-- let printExceptionStep :: HaskellDevFailed Text -> Text
--     printExceptionStep (HaskellDevFailed {devFailedDesc, devFailedReason, devFailedOrigin, devFailedSeverity}) = devFailedDesc <> ", " <> devFailedReason
--     printTangoException :: TangoException -> IO ()
--     printTangoException (TangoException steps) =
--       putStrLn $ intercalate "\n" $ printExceptionStep <$> steps
-- catch (tangoServerStart initedServer) printTangoException
