{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack, strip, unpack)
import Data.Text.IO (putStrLn)
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
    HaskellTangoDevState (Unknown),
  )
import Tango.Server
  ( CommandCallback,
    DeviceInitCallback,
    DeviceInstancePtr,
    HaskellAttributeDefinition (..),
    HaskellCommandDefinition (..),
    createCommandCallback,
    createDeviceInitCallback,
    createFnWrapper,
    createGlobalFinalizer,
    tango_server_add_attribute_definition,
    tango_server_add_command_definition,
    tango_server_add_property,
    tango_server_init,
    tango_server_read_property,
    tango_server_set_status,
    tango_server_start,
  )
import TangoHL (CommandName (CommandName), DeviceProxyPtr, PropertyName (PropertyName), ServerStatus (ServerStatus), TangoServerCommand (ServerCommandVoidVoid), newDeviceProxy, readDoubleAttribute, tangoReadProperty, tangoServerInit, tangoServerStart, tangoUrlFromText)
import qualified UnliftIO
import UnliftIO.Foreign (peekCString, with, withArray, withCString)
import Prelude hiding (putStrLn)

data DeviceData = DeviceData
  { deviceDataDetectorTower :: DeviceProxyPtr
  }

prepareForMeasurement :: MVar DeviceData -> DeviceInstancePtr -> IO ()
prepareForMeasurement data' _instance = do
  readData <- readMVar data'
  double <- readDoubleAttribute (deviceDataDetectorTower readData) "DetectorDistanceLaser"
  putStrLn $ "laser distance: " <> pack (show double)

propDetectorTowerIdentifier :: PropertyName
propDetectorTowerIdentifier = PropertyName "detector_tower_identifier"

initCallback :: MVar DeviceData -> DeviceInstancePtr -> IO ()
initCallback deviceData instance' = do
  putStrLn "in init callback, connecting to detector tower"
  detectorTowerIdentifier <- tangoReadProperty instance' propDetectorTowerIdentifier
  detectorTowerProxy <- newDeviceProxy (tangoUrlFromText detectorTowerIdentifier)
  putMVar deviceData (DeviceData detectorTowerProxy)
  putStrLn "done"

main :: IO ()
main = do
  deviceData <- newEmptyMVar
  initedServerEither <-
    tangoServerInit
      [propDetectorTowerIdentifier]
      (ServerStatus "")
      Unknown
      [ServerCommandVoidVoid (CommandName "prepare_for_measurement") (prepareForMeasurement deviceData)]
      (initCallback deviceData)
  case initedServerEither of
    Left e -> putStrLn ("error initializing: " <> e)
    Right initedServer -> tangoServerStart initedServer
