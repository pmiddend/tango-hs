{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify)
import Data.Text (Text, pack, strip, unpack)
import Data.Text.IO (putStrLn)
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
    HaskellTangoDevState (Running, Unknown),
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
    PropertyName (PropertyName),
    ServerStatus (ServerStatus),
    TangoServerAttribute (TangoServerAttribute, tangoServerAttributeAccessor, tangoServerAttributeName),
    TangoServerAttributeAccessor (TangoServerAttributeAccessor),
    TangoServerAttributeTypes (TangoServerAttributeTypeString),
    TangoServerCommand (ServerCommandVoidVoid),
    newDeviceProxy,
    readDoubleAttribute,
    readUShortAttribute,
    tangoReadProperty,
    tangoServerInit,
    tangoServerStart,
    tangoUrlFromText,
  )
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
    outZ :: Double
  }

instance FromJSON ColliConfig where
  parseJSON = withObject "ColliConfig" \v -> ColliConfig <$> v .: "in_y" <*> v .: "in_z" <*> v .: "out_y" <*> v .: "out_z"

readColliConfig :: FilePath -> Either Text ColliConfig
readColliConfig fp = case eitherDecodeFileStrict' fp of
  Left e -> Left (pack e)
  Right v -> Right v

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
    eigerDetector :: !DeviceProxyPtr,
  }

data DesiredCollimatorStatus = DesiredIn | DesiredOut

data DeviceData = DeviceData
  { proxies :: Proxies,
    detectorTowerMeasurementDistanceMm :: Double,
    desiredCollimatorStatus :: DesiredCollimatorStatus,
    calculatedRunnerState :: RunnerState
  }

prepareForMeasurement :: MVar DeviceData -> DeviceInstancePtr -> IO ()
prepareForMeasurement data' _instance = pure ()

propDetectorTowerIdentifier :: PropertyName
propDetectorTowerIdentifier = PropertyName "detector_tower_identifier"

numberIsClose a b relTol absTol =
  abs (a - b) <= max (relTol * max (abs a) (abs b)) absTol

numberIsCloseAbs a b = numberIsClose a b 1e-9

flippedEvalState m initial = evalStateT initial m

finishUnknownStatic appendix = do
  messagesSoFar <- get
  pure $ UnknownStatic $ intercalate " and " messagesSoFar <> appendix

checkCollimator collimatorY collimatorZ colliConfig colliPositionToleranceY colliPositionToleranceZ = do
  let checkSubdevice proxy name inPos outPos tolerance = do
        pos <- readDoubleValue proxy "Position"
        case desiredColliStatus of
          DesiredIn ->
            if not (numberIsCloseAbs pos inPos tolerance)
              then Just <$> finishUnknownStatic (", but the collimator " <> name <> " is not in the desired position (in)")
              else pure Nothing
          DesiredOut ->
            if not (numberIsCloseAbs pos outPos tolerance)
              then Just <$> finishUnknownStatic (", but the collimator " <> name <> " is not in the desired position (out)")
              else pure Nothing
  yStatus <- checkSubdevice collimatorY "y" colliConfig.inY colliPositionToleranceY
  zStatus <- checkSubdevice collimatorZ "z" colliConfig.inZ colliPositionToleranceZ
  pure (yStatus <|> zStatus)

readFastShutterOpen fastShutterProxy = do
  fastShutterValue <- readUShortAttribute fastShutterProxy "PSOoutputStatus"
  pure (fastShutterValue /= 0)

calculateState :: Proxies -> Double -> Double -> DesiredCollimatorStatus -> ColliConfig -> IO RunnerState
calculateState (Proxies {detectorTower, chopper, eigerStream, fastShutter, eigerDetector}) towerMeasurementDistanceMm detectorDistanceToleranceMm desiredColliStatus colliConfig colliPositionToleranceY colliPositionToleranceZ = do
  towerState <- readStateAttribute detectorTower
  chopperState <- readStateAttribute chopper
  shieldIsDown <- readBoolAttribute detectorTower "ShieldIsDown"
  shieldIsUp <- readBoolAttribute detectorTower "ShieldIsUp"
  collimatorYState <- readStateAttribute collimatorY
  collimatorZState <- readStateAttribute collimatorZ
  if towerState /= On || collimatorY == Moving || collimatorZ == Moving || chopperState /= On || shieldIsUp /= shieldIsDown
    then
      pure $
        Moving $
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
      let addMessage message = modify (\xs -> message :: xs)
      towerDistanceMm <- readDoubleAttribute "DetectorDistance"
      if numberIsCloseAbs towerDistanceMm towerMeasurementDistanceMm detectorDistanceToleranceMm
        then do
          addMessage "detector tower is in measuring distance"
          colliResult <- checkCollimator collimatorY collimatorZ colliConfig colliPositionToleranceY colliPositionToleranceZ
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
                        else pure ReadyToMeasure
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
                          eigerConfigurationCheck <- checkEigerConfiguration eigerDetector eigerStream
                          case eigerConfigurationCheck of
                            Just v -> pure v
                            Nothing -> do
                              addMessage "Eiger detector is configured correctly"
                              -- FIXME: continue here
        else addMessage "lol please continue here"

initCallback :: MVar DeviceData -> DeviceInstancePtr -> IO ()
initCallback deviceData instance' = do
  putStrLn "in init callback, connecting to detector tower"
  detectorTowerIdentifier <- tangoReadProperty instance' propDetectorTowerIdentifier
  detectorTowerProxy <- newDeviceProxy (tangoUrlFromText detectorTowerIdentifier)
  let proxies = Proxies {detectorTower = detectorTowerProxy}
  calculatedState <- calculateState proxies
  putMVar deviceData (DeviceData {proxies = proxies, calculatedRunnerState = calculatedState})
  putStrLn "done"

readTestAttribute :: DeviceInstancePtr -> IO Text
readTestAttribute _ = pure "lol"

main :: IO ()
main = do
  deviceData <- newEmptyMVar
  initedServerEither <-
    tangoServerInit
      [propDetectorTowerIdentifier]
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
