{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (putStrLn)
import Foreign.C (CInt, CLong)
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.Marshal (free, peekArray, with)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (new)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import System.Environment (getArgs, getProgName)
import Tango.Common
  ( HaskellAttrWriteType (Read, ReadWrite),
    HaskellTangoDataType (HaskellDevBoolean, HaskellDevLong64, HaskellDevString, HaskellDevVoid),
    HaskellTangoDevState (Moving),
  )
import Tango.Server
  ( HaskellAttributeDefinition (..),
    HaskellCommandDefinition (..),
    createCommandCallback,
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
import Prelude hiding (putStrLn)

data InitedServer = InitedServer

newtype PropertyName = PropertyName {getPropertyName :: Text}

newtype ServerStatus = ServerStatus Text

withCStringFromText :: Text -> (CString -> IO a) -> IO a
withCStringFromText t = withCString (unpack t)

tangoServerInit :: [PropertyName] -> ServerStatus -> HaskellTangoDevState -> IO (Either Text InitedServer)
tangoServerInit propertyNames (ServerStatus initialStatus) initialState = do
  progName <- getProgName
  args <- getArgs
  freeFinalizerWrapped <- createGlobalFinalizer free
  case args of
    [] -> pure (Left "cannot initialize device server, missing first argument (instance name)")
    (instanceName : _) ->
      withCString progName \progNameC' ->
        withCString instanceName \instanceNameC' ->
          withArray [progNameC', instanceNameC'] \programArgumentsC ->
            withCString (unpack initialStatus) \initialStatusC -> do
              mapM_
                (\propName -> withCString propName tango_server_add_property)
                ((unpack . getPropertyName) <$> propertyNames)
              tango_server_init
                2
                programArgumentsC
                freeFinalizerWrapped
                initialStatusC
                (fromIntegral $ fromEnum initialState)
              pure (Right InitedServer)

tangoServerStart :: InitedServer -> IO ()
tangoServerStart _initedServer = tango_server_start

tangoReadProperty :: InitedServer -> PropertyName -> IO Text
tangoReadProperty _initedServer (PropertyName n) = do
  resultAsCString <- withCStringFromText n tango_server_read_property
  resultAsString <- peekCString resultAsCString
  pure (pack resultAsString)

main :: IO ()
main = do
  let propDetectorIdentifier = PropertyName "detector_identifier"
  initedServerEither <- tangoServerInit [propDetectorIdentifier] (ServerStatus "") Moving
  case initedServerEither of
    Left e -> putStrLn ("error initializing: " <> e)
    Right initedServer -> do
      detectorIdentifier <- tangoReadProperty initedServer propDetectorIdentifier
      putStrLn ("detector identifier: " <> detectorIdentifier)
      tangoServerStart initedServer
