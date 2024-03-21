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
import TangoHL (DeviceProxyPtr, newDeviceProxy, readDoubleAttribute, tangoUrlFromText)
import qualified UnliftIO
import UnliftIO.Foreign (peekCString, with, withArray, withCString)
import Prelude hiding (putStrLn)

data InitedServer = InitedServer

newtype PropertyName = PropertyName {getPropertyName :: Text}

newtype ServerStatus = ServerStatus Text

withCStringFromText :: (UnliftIO.MonadUnliftIO m) => Text -> (CString -> m a) -> m a
withCStringFromText t = withCString (unpack t)

newtype CommandName = CommandName Text

data TangoServerCommand = ServerCommandVoidVoid CommandName (DeviceInstancePtr -> IO ())

data DeviceData = DeviceData
  { deviceDataDetectorTower :: DeviceProxyPtr
  }

prepareForMeasurement :: MVar DeviceData -> DeviceInstancePtr -> IO ()
prepareForMeasurement data' _instance = do
  readData <- readMVar data'
  double <- readDoubleAttribute (deviceDataDetectorTower readData) "DetectorDistanceLaser"
  putStrLn $ "laser distance: " <> pack (show double)

tangoServerInit ::
  (UnliftIO.MonadUnliftIO m) =>
  [PropertyName] ->
  ServerStatus ->
  HaskellTangoDevState ->
  [TangoServerCommand] ->
  DeviceInitCallback ->
  m (Either Text InitedServer)
tangoServerInit propertyNames (ServerStatus initialStatus) initialState commands deviceInitCallback = do
  progName <- liftIO getProgName
  args <- liftIO getArgs
  freeFinalizerWrapped <- liftIO (createGlobalFinalizer free)
  deviceInitCallbackWrapped <- liftIO (createDeviceInitCallback deviceInitCallback)
  let voidWrapped :: (DeviceInstancePtr -> IO ()) -> CommandCallback
      voidWrapped voidFunction instance' _ptrToBeIgnored = do
        voidFunction instance'
        pure nullPtr
      extractCommandName :: TangoServerCommand -> Text
      extractCommandName (ServerCommandVoidVoid (CommandName n) _) = n
      wrapCommand :: TangoServerCommand -> CommandCallback
      wrapCommand (ServerCommandVoidVoid _name f) = voidWrapped f
      withConvertedCommand tsc f = do
        wrappedCommandCallback <- liftIO (createCommandCallback (wrapCommand tsc))
        withCString (unpack (extractCommandName tsc)) \commandNameC ->
          with
            ( HaskellCommandDefinition
                commandNameC
                HaskellDevVoid
                HaskellDevVoid
                wrappedCommandCallback
            )
            f
  forM_ commands \haskellCommand -> withConvertedCommand haskellCommand (liftIO . tango_server_add_command_definition)
  -- commandsWrapped <- traverse () commands
  -- forM_ commands \haskellCommand -> do
  --   wrappedCommandCallback <- liftIO (createCommandCallback (wrapCommand haskellCommand))
  --   withCString (unpack commandName) \commandNameC ->
  --     with
  --       ( HaskellCommandDefinition
  --           commandNameC
  --           HaskellDevVoid
  --           HaskellDevVoid
  --           prepareForMeasurementWrapped
  --       )
  --       \commandDefPtr ->
  --         liftIO
  --           ( tango_server_add_command_definition
  --               commandDefPtr
  --           )
  case args of
    [] -> pure (Left "cannot initialize device server, missing first argument (instance name)")
    (instanceName : _) -> do
      liftIO $ putStrLn $ "initializing server with class name " <> pack progName <> ", instance name " <> pack instanceName
      withCString progName \progNameC' ->
        withCString instanceName \instanceNameC' ->
          withArray [progNameC', instanceNameC'] \programArgumentsC ->
            withCString (unpack initialStatus) \initialStatusC -> do
              mapM_
                (\propName -> withCString propName (\x -> liftIO (tango_server_add_property x)))
                ((unpack . getPropertyName) <$> propertyNames)
              liftIO $
                tango_server_init
                  2
                  programArgumentsC
                  freeFinalizerWrapped
                  initialStatusC
                  (fromIntegral $ fromEnum initialState)
                  deviceInitCallbackWrapped
              pure (Right InitedServer)

tangoServerStart :: (UnliftIO.MonadUnliftIO m) => InitedServer -> m ()
tangoServerStart _initedServer = liftIO tango_server_start

tangoReadProperty :: (UnliftIO.MonadUnliftIO m) => DeviceInstancePtr -> PropertyName -> m Text
tangoReadProperty instance' (PropertyName n) = do
  resultAsCString <- withCStringFromText n (\x -> liftIO (tango_server_read_property instance' x))
  resultAsString <- peekCString resultAsCString
  pure (strip (pack resultAsString))

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
