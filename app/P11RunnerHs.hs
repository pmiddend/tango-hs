{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, putMVar, readMVar)
import Control.Monad (forM_, when)
import Control.Monad.Free (Free)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, gets, modify)
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), eitherDecodeFileStrict', object, withObject, (.:))
import Data.Fixed (Pico)
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq, ViewR (EmptyR, (:>)), viewr, (<|))
import Data.String (IsString)
import Data.Text (Text, intercalate, pack, strip, unpack)
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

data RunnerState
  = RunnerStateUnknownMoving !Text
  | RunnerStateUnknownStatic !Text
  | RunnerStateStoppingChopper !ChopperState
  | RunnerStateMeasuring !MeasuringState
  | RunnerStatePreparingForMeasurement !(Maybe UTCTime)
  | RunnerStateReadyToMeasure
  | RunnerStatePreparingToOpenHutch !(Maybe UTCTime)
  | ReadyToOpenHutch

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
    towerSafeDistanceMm :: Double,
    towerMeasurementDistanceMm :: Double,
    desiredCollimatorStatus :: DesiredCollimatorStatus,
    calculatedRunnerState :: RunnerState,
    colliConfig :: ColliConfig,
    useChopper :: Bool,
    exposureTimeMs :: Double,
    numberOfImages :: Int,
    towerDistanceToleranceMm :: Double,
    msgTrace :: Seq Message
  }

data P11RunnerState = P11RunnerState
  { runnerVar :: MVar DeviceData,
    runnerInstance :: DeviceInstancePtr
  }

type P11RunnerMonad = StateT P11RunnerState IO

readDeviceData :: P11RunnerMonad DeviceData
readDeviceData = do
  runnerVar' <- gets runnerVar
  liftIO (readMVar runnerVar')

readVar :: P11RunnerMonad (MVar DeviceData)
readVar = gets runnerVar

commandUpdate :: MVar DeviceData -> DeviceInstancePtr -> IO ()
commandUpdate data' _instance = do
  currentData <- readMVar data'
  newState <- calculateState currentData
  modifyMVar_ data' (\existing -> pure existing {calculatedRunnerState = newState})

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

markdownPlain :: Text -> Text
markdownPlain = id

markdownBold :: (Semigroup a, IsString a) => a -> a
markdownBold x = "**" <> x <> "**"

markdownEmph :: (Semigroup a, IsString a) => a -> a
markdownEmph x = "*" <> x <> "*"

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

moveColliToDesired :: P11RunnerMonad ()
moveColliToDesired = do
  currentData <- readDeviceData
  let positionY = if currentData.desiredCollimatorStatus == DesiredIn then currentData.colliConfig.inY else currentData.colliConfig.outY
      positionZ = if currentData.desiredCollimatorStatus == DesiredIn then currentData.colliConfig.inZ else currentData.colliConfig.outZ
  appendMsg LogLevelInfo ("moving " <> markdownBold "collimator" <> ", this might take a while")
  liftIO $ writeDoubleAttribute currentData.proxies.collimatorY "Position" positionY
  liftIO $ writeDoubleAttribute currentData.proxies.collimatorZ "Position" positionZ

readDetectorDistance :: (MonadIO m) => DeviceData -> m Double
readDetectorDistance currentData = liftIO $ readDoubleAttribute currentData.proxies.detectorTower "DetectorDistance"

writeDetectorDistance :: (MonadIO m) => DeviceData -> Double -> m ()
writeDetectorDistance currentData distance = liftIO $ writeDoubleAttribute currentData.proxies.detectorTower "DetectorDistance" distance

detectorInMeasurementDistance :: DeviceData -> Double -> Bool
detectorInMeasurementDistance currentData towerDistanceMm =
  numberIsCloseAbs towerDistanceMm currentData.towerMeasurementDistanceMm currentData.towerDistanceToleranceMm

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

prepareForMeasurement :: P11RunnerMonad ()
prepareForMeasurement = do
  currentData <- readDeviceData
  case currentData.calculatedRunnerState of
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

      towerDistanceMm <- readDetectorDistance currentData

      if detectorInMeasurementDistance currentData towerDistanceMm
        then
          appendMsg
            LogLevelInfo
            (markdownBold "tower" <> " already in right position")
        else do
          appendMsg
            LogLevelInfo
            ("moving " <> markdownBold "tower" <> " to " <> markdownEmph (packShow currentData.towerMeasurementDistanceMm) <> "mm")
          writeDetectorDistance currentData currentData.towerMeasurementDistanceMm

      shieldIsDown' <- shieldIsDown

      if shieldIsDown'
        then raiseShield
        else appendMsg LogLevelDebug (markdownBold "shield" <> markdownPlain " already up")

      fastShutterIsOpen' <- fastShutterIsOpen

      if fastShutterIsOpen'
        then closeFastShutter
        else appendMsg LogLevelDebug (markdownBold "fast shutter" <> markdownPlain " already closed")

numberIsClose :: (Ord a, Num a) => a -> a -> a -> a -> Bool
numberIsClose a b relTol absTol =
  abs (a - b) <= max (relTol * max (abs a) (abs b)) absTol

numberIsCloseAbs :: (Ord a, Fractional a) => a -> a -> a -> Bool
numberIsCloseAbs a b = numberIsClose a b 1e-9

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
                    towerSafeDistanceMm,
                    towerMeasurementDistanceMm,
                    desiredCollimatorStatus,
                    towerDistanceToleranceMm,
                    colliConfig
                  }
                ) = do
    towerState <- readStateAttribute detectorTower
    chopperState <- readStateAttribute chopper
    shieldIsDown <- readBoolAttribute detectorTower "ShieldIsDown"
    shieldIsUp <- readBoolAttribute detectorTower "ShieldIsUp"
    collimatorYState <- readStateAttribute collimatorY
    collimatorZState <- readStateAttribute collimatorZ
    if towerState /= On || collimatorYState == Moving || collimatorZState == Moving || chopperState /= On || shieldIsUp /= shieldIsDown
      then
        pure $
          RunnerStateUnknownMoving $
            "tower state (has to be ON) is "
              <> packShow towerState
              <> ", collimator y/z movement states (have to be non-moving) are "
              <> packShow collimatorYState
              <> "/"
              <> packShow collimatorZState
              <> ", chopper state (has to be ON) is "
              <> packShow chopperState
              <> " and shield down/up booleans are "
              <> packShow shieldIsDown
              <> "/"
              <> packShow shieldIsUp
      else flippedEvalState ["nothing is moving"] do
        let addMessage message = modify (message :)
        towerDistanceMm <- readDetectorDistance currentData
        if detectorInMeasurementDistance currentData towerDistanceMm
          then do
            addMessage "detector tower is in measuring distance"
            colliResult <- checkCollimator collimatorY collimatorZ colliConfig desiredCollimatorStatus
            case colliResult of
              AtLeastOneColliNotDesired (colli NE.:| []) -> finishUnknownStatic (", but the colli " <> colli <> " is not in desired position")
              AtLeastOneColliNotDesired _ -> finishUnknownStatic ", but both collimator axes are not in the dseired position"
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
            if not (numberIsCloseAbs towerDistanceMm towerSafeDistanceMm towerDistanceToleranceMm)
              then
                finishUnknownStatic
                  ( ", but the tower is neither in the safe distance nor in the measurement distance: "
                      <> packShow towerDistanceMm
                      <> " (safe distance is "
                      <> packShow towerSafeDistanceMm
                      <> ", measurement distance is "
                      <> packShow towerMeasurementDistanceMm
                      <> ")"
                  )
              else do
                addMessage "tower is in safe distance"
                if shieldIsDown
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
    propColliConfig :: FilePath,
    propColliMovementTimeoutS :: Double
  }

readMaybeText :: Text -> Maybe Double
readMaybeText = readMaybe . unpack

p11RunnerProperties :: PropApplicative P11RunnerProperties
p11RunnerProperties =
  P11RunnerProperties
    <$> readTypedProperty "detector_tower_identifier" (Just . tangoUrlFromText)
    <*> readTypedProperty "chopper_identifier" (Just . tangoUrlFromText)
    <*> readTypedProperty "colli_motor_identifier_y" (Just . tangoUrlFromText)
    <*> readTypedProperty "colli_motor_identifier_z" (Just . tangoUrlFromText)
    <*> readTypedProperty "eiger_stream_identifier" (Just . tangoUrlFromText)
    <*> readTypedProperty "fast_shutter_identifier" (Just . tangoUrlFromText)
    <*> readTypedProperty "detector_identifier" (Just . tangoUrlFromText)
    <*> readTypedProperty "colli_position_tolerance_y" readMaybeText
    <*> readTypedProperty "colli_position_tolerance_z" readMaybeText
    <*> readTypedProperty "detector_tower_safe_distance_mm" readMaybeText
    <*> readTypedProperty "detector_distance_tolerance_mm" readMaybeText
    <*> readTypedProperty "colli_config" (Just . unpack)
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
            propColliConfig,
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
        colliConfig <- readColliConfig propColliConfig
        case colliConfig of
          Left e -> error $ "invalid colli config file \"" <> propColliConfig <> "\": " <> unpack e
          Right colliConfig' -> do
            let deviceDataContent =
                  DeviceData
                    { proxies = proxies,
                      towerSafeDistanceMm = propTowerSafeDistanceMm,
                      towerMeasurementDistanceMm = 200.0,
                      desiredCollimatorStatus = DesiredIn,
                      calculatedRunnerState = RunnerStateUnknownMoving "before first connect call",
                      useChopper = False,
                      exposureTimeMs = 7.6,
                      numberOfImages = 1000,
                      colliConfig = colliConfigFromJson colliConfig' propColliMovementTimeoutS propColliToleranceY propColliToleranceZ,
                      towerDistanceToleranceMm = propTowerDistanceToleranceMm,
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
          (commandUpdate deviceData),
        ServerCommandVoidVoid
          (CommandName "prepare_for_measurement")
          ( \instancePtr ->
              evalStateT
                prepareForMeasurement
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
