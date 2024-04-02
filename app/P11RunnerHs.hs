{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (forM_)
import Control.Monad.Free (Free)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeFileStrict', withObject, (.:))
import Data.Text (Text, intercalate, pack, strip, unpack)
import Data.Text.IO (putStrLn)
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime)
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
    toleranceZ :: Double
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

colliConfigFromJson :: ColliConfigJson -> Double -> Double -> ColliConfig
colliConfigFromJson (ColliConfigJson {inY, inZ, outY, outZ}) toleranceY toleranceZ =
  ColliConfig
    { inY = inY,
      inZ = inZ,
      outY = outY,
      outZ = outZ,
      toleranceY = toleranceY,
      toleranceZ = toleranceZ
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

data DesiredCollimatorStatus = DesiredIn | DesiredOut

data DeviceData = DeviceData
  { proxies :: Proxies,
    detectorTowerSafeDistanceMm :: Double,
    detectorTowerMeasurementDistanceMm :: Double,
    desiredCollimatorStatus :: DesiredCollimatorStatus,
    calculatedRunnerState :: RunnerState,
    colliConfig :: ColliConfig,
    useChopper :: Bool,
    exposureTimeMs :: Double,
    numberOfImages :: Int,
    towerDistanceToleranceMm :: Double
  }

prepareForMeasurement :: MVar DeviceData -> DeviceInstancePtr -> IO ()
prepareForMeasurement data' _instance = pure ()

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

checkCollimator ::
  forall m.
  (MonadIO m) =>
  DeviceProxyPtr ->
  DeviceProxyPtr ->
  ColliConfig ->
  DesiredCollimatorStatus ->
  Double ->
  Double ->
  StateT [Text] m (Maybe RunnerState)
checkCollimator collimatorY collimatorZ colliConfig desiredColliStatus colliPositionToleranceY colliPositionToleranceZ = do
  let checkSubdevice :: DeviceProxyPtr -> Text -> Double -> Double -> Double -> StateT [Text] m (Maybe RunnerState)
      checkSubdevice proxy name inPos outPos tolerance = do
        pos <- readDoubleAttribute proxy "Position"
        case desiredColliStatus of
          DesiredIn ->
            if not (numberIsCloseAbs pos inPos tolerance)
              then Just <$> finishUnknownStatic (", but the collimator " <> name <> " is not in the desired position (in)")
              else pure Nothing
          DesiredOut ->
            if not (numberIsCloseAbs pos outPos tolerance)
              then Just <$> finishUnknownStatic (", but the collimator " <> name <> " is not in the desired position (out)")
              else pure Nothing
  yStatus <- checkSubdevice collimatorY "y" colliConfig.inY colliConfig.outY colliPositionToleranceY
  zStatus <- checkSubdevice collimatorZ "z" colliConfig.inZ colliConfig.outZ colliPositionToleranceZ
  pure (yStatus <|> zStatus)

readFastShutterOpen :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> m Bool
readFastShutterOpen fastShutterProxy = do
  fastShutterValue <- readUShortAttribute fastShutterProxy "PSOoutputStatus"
  pure (fastShutterValue /= 0)

isEigerConfigured :: Bool -> Double -> Int -> DeviceProxyPtr -> DeviceProxyPtr -> DeviceProxyPtr -> m Bool
isEigerConfigured useChopper userExposureTimeMs numberOfImages chopperProxy eigerDetector eigerStream = do
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
    numberIsCloseAbs (frameTimeS * 1000.0) desiredFrameTimeMs 0.1
      && numberIsCloseAbs (countTimeS * 1000.0) desiredCountTimeMs 0.1
      && triggerMode == desiredTriggerMode
      && nimages == desiredNimages
      && ntrigger == desiredNtrigger

-- tangoReadDoubleProperty :: (UnliftIO.MonadUnliftIO m, Read b) => DeviceInstancePtr -> PropertyName -> m b
-- tangoReadDoubleProperty instance' propName@(PropertyName propName') = do
--   stringValue <- tangoReadProperty instance' propName
--   case readMaybe (unpack stringValue) of
--     Nothing -> error $ "couldn't read double attribute " <> unpack propName' <> ": couldn't convert this from string: " <> unpack stringValue
--     Just doubleValue -> pure doubleValue

packShow :: (Show a) => a -> Text
packShow = pack . show

calculateState :: Proxies -> Bool -> Double -> Int -> Double -> Double -> DesiredCollimatorStatus -> ColliConfig -> Double -> Double -> IO RunnerState
calculateState
  ( Proxies
      { detectorTower,
        chopper,
        eigerStream,
        fastShutter,
        eigerDetector,
        collimatorY,
        collimatorZ
      }
    )
  useChopper
  userExposureTimeMs
  numberOfImages
  towerMeasurementDistanceMm
  detectorDistanceToleranceMm
  desiredColliStatus
  colliConfig
  colliPositionToleranceY
  colliPositionToleranceZ = do
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
        towerDistanceMm <- readDoubleAttribute detectorTower "DetectorDistance"
        if numberIsCloseAbs towerDistanceMm towerMeasurementDistanceMm detectorDistanceToleranceMm
          then do
            addMessage "detector tower is in measuring distance"
            colliResult <-
              checkCollimator
                collimatorY
                collimatorZ
                colliConfig
                desiredColliStatus
                colliPositionToleranceY
                colliPositionToleranceZ
            case colliResult of
              Just v -> pure v
              Nothing -> do
                addMessage "collimator is in desired position"
                if shieldIsUp
                  then do
                    streamStatus <- readStateAttribute eigerStream
                    case streamStatus of
                      Running -> finishUnknownStatic ", but the shield is up and the Eiger is busy (have you forgotten to interlock the hutch?)"
                      _ -> do
                        addMessage "Eiger is not busy"
                        fastShutterOpen <- readFastShutterOpen fastShutter
                        if fastShutterOpen
                          then finishUnknownStatic ", but the fast shutter is open"
                          else pure RunnerStateReadyToMeasure
                  else do
                    addMessage "shield is down"
                    streamStatus <- readStateAttribute eigerStream

                    if streamStatus /= Running
                      then -- This might happen during the diode measurement which
                      -- hasn't completed yet, I suppose? But then we're in
                      -- Measuring anyways, so this shouldn't disturb anyone.
                        finishUnknownStatic ", but the stream is not busy (wait a few seconds - if it persists, then we have a problem)"
                      else do
                        addMessage "stream is busy"
                        fastShutterOpen <- readFastShutterOpen fastShutter
                        if not fastShutterOpen
                          then finishUnknownStatic ", but the fast shutter is not open"
                          else do
                            eigerConfigurationCheck :: Bool <-
                              isEigerConfigured
                                useChopper
                                userExposureTimeMs
                                numberOfImages
                                chopper
                                eigerDetector
                                eigerStream
                            case eigerConfigurationCheck of
                              Just v -> pure v
                              Nothing -> do
                                addMessage "Eiger detector is configured correctly"
          else -- FIXME: continue here
            addMessage "lol please continue here"

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
    propColliConfig :: FilePath
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
            propColliToleranceZ
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
                      detectorTowerSafeDistanceMm = propTowerSafeDistanceMm,
                      detectorTowerMeasurementDistanceMm = 200.0,
                      desiredCollimatorStatus = DesiredIn,
                      calculatedRunnerState = RunnerStateUnknownMoving "before first connect call",
                      useChopper = False,
                      exposureTimeMs = 7.6,
                      numberOfImages = 1000,
                      colliConfig = colliConfigFromJson colliConfig' propColliToleranceY propColliToleranceZ,
                      towerDistanceToleranceMm = propTowerDistanceToleranceMm
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
      [ServerCommandVoidVoid (CommandName "prepare_for_measurement") (prepareForMeasurement deviceData)]
      (initCallback deviceData)
  case initedServerEither of
    Left e -> putStrLn ("error initializing: " <> e)
    Right initedServer -> tangoServerStart initedServer
