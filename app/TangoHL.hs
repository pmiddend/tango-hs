{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TangoHL
  ( withDeviceProxy,
    checkResult,
    readStringAttribute,
    writeIntAttribute,
    commandInOutVoid,
    newDeviceProxy,
    readLong64Attribute,
    readUShortAttribute,
    readDoubleAttribute,
    readBoolAttribute,
    readStateAttribute,
    tangoUrlFromText,
    DeviceProxyPtr,
    PropertyName (PropertyName),
    TangoServerAttribute (TangoServerAttribute, tangoServerAttributeName, tangoServerAttributeAccessor),
    TangoServerAttributeTypes (TangoServerAttributeTypeString),
    TangoServerAttributeAccessor (TangoServerAttributeAccessor),
    CommandName (CommandName),
    AttributeName (AttributeName),
    ServerStatus (ServerStatus),
    TangoServerCommand (..),
    tangoServerStart,
    tangoReadProperty,
    tangoServerInit,
    InitedServer,
  )
where

import Control.Applicative (pure)
import Control.Exception (Exception, bracket, throw)
import Control.Monad (fail, forM_, mapM_, void, when, (>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool)
import Data.Char (Char)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)), (/=))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int, Int16, Int32, Int64)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import Data.Text (Text, pack, strip, unpack)
import Data.Text.IO (putStrLn)
import Data.Traversable (traverse)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (free)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (IO)
import Tango.Common
  ( DeviceProxyPtr,
    HaskellAttrWriteType (Read, ReadWrite),
    HaskellAttributeData (..),
    HaskellAttributeDataList (attributeDataListSequence),
    HaskellCommandData (..),
    HaskellDataFormat (..),
    HaskellDataQuality (..),
    HaskellDevFailed (HaskellDevFailed),
    HaskellErrorStack (errorStackLength, errorStackSequence),
    HaskellTangoAttributeData (HaskellAttributeDataBoolArray, HaskellAttributeDataDoubleArray, HaskellAttributeDataLong64Array, HaskellAttributeDataLongArray, HaskellAttributeDataStateArray, HaskellAttributeDataStringArray, HaskellAttributeDataUShortArray),
    HaskellTangoCommandData (..),
    HaskellTangoDataType (..),
    HaskellTangoDevState,
    HaskellTangoVarArray (..),
    Timeval (..),
    tango_command_inout,
    tango_create_device_proxy,
    tango_delete_device_proxy,
    tango_free_AttributeData,
    tango_free_CommandData,
    tango_get_timeout_millis,
    tango_read_attribute,
    tango_set_timeout_millis,
    tango_write_attribute,
  )
import Tango.Server
  ( CommandCallback,
    DeviceInitCallback,
    DeviceInstancePtr,
    HaskellAttributeDefinition (HaskellAttributeDefinition),
    HaskellAttributeGetter,
    HaskellAttributeSetter,
    HaskellCommandDefinition (HaskellCommandDefinition),
    createCommandCallback,
    createDeviceInitCallback,
    createGetterWrapper,
    createGlobalFinalizer,
    createSetterWrapper,
    tango_server_add_attribute_definition,
    tango_server_add_command_definition,
    tango_server_add_property,
    tango_server_init,
    tango_server_read_property,
    tango_server_start,
  )
import Text.Show (Show, show)
import qualified UnliftIO
import UnliftIO.Environment (getArgs, getProgName)
import UnliftIO.Foreign (FunPtr, alloca, castPtr, newCString, peek, peekArray, peekCString, poke, with, withArray, withCString)
import Prelude (Double, Enum (fromEnum), Float, error, fromIntegral, realToFrac)

newtype TangoException = TangoException [HaskellDevFailed Text] deriving (Show)

instance Exception TangoException

checkResult :: (UnliftIO.MonadUnliftIO m) => m (Ptr HaskellErrorStack) -> m ()
checkResult action = do
  es <- action
  when (es /= nullPtr) $ do
    errorStack <- liftIO $ peek es
    stackItems <- peekArray (fromIntegral (errorStackLength errorStack)) (errorStackSequence errorStack)
    formattedStackItems :: [HaskellDevFailed Text] <- traverse (traverse ((pack <$>) . peekCString)) stackItems
    throw (TangoException formattedStackItems)

newtype TangoUrl = TangoUrl Text

tangoUrlFromText :: Text -> TangoUrl
tangoUrlFromText = TangoUrl

newDeviceProxy :: forall m. (UnliftIO.MonadUnliftIO m) => TangoUrl -> m DeviceProxyPtr
newDeviceProxy (TangoUrl url) =
  alloca $ \proxyPtrPtr -> do
    withCString (unpack url) $ \proxyName -> do
      liftIO $ checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
      liftIO $ peek proxyPtrPtr

withDeviceProxy :: forall m a. (UnliftIO.MonadUnliftIO m) => Text -> (DeviceProxyPtr -> m a) -> m a
withDeviceProxy proxyAddress =
  let initialize :: m DeviceProxyPtr
      initialize =
        liftIO $ alloca $ \proxyPtrPtr -> do
          withCString (unpack proxyAddress) $ \proxyName -> do
            checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
            peek proxyPtrPtr
      deinitialize :: DeviceProxyPtr -> m ()
      deinitialize proxyPtrPtr =
        liftIO $ checkResult (tango_delete_device_proxy proxyPtrPtr)
   in UnliftIO.bracket initialize deinitialize

writeIntAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> Int -> m ()
writeIntAttribute proxyPtr attributeName newValue = do
  withCString (unpack attributeName) $ \attributeNameC ->
    with (fromIntegral newValue) $ \newValuePtr -> with
      ( HaskellAttributeData
          { dataFormat = HaskellScalar,
            dataQuality = HaskellValid,
            nbRead = 0,
            name = attributeNameC,
            dimX = 1,
            dimY = 1,
            timeStamp = Timeval 0 0,
            dataType = HaskellDevLong64,
            tangoAttributeData = HaskellAttributeDataLongArray (HaskellTangoVarArray 1 newValuePtr)
          }
      )
      $ \newDataPtr ->
        liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

readStringAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> m Text
readStringAttribute proxyPtr attributeNameHaskell =
  liftIO $ withCString (unpack attributeNameHaskell) $ \attributeName -> do
    alloca $ \haskellAttributeDataPtr -> do
      checkResult (tango_read_attribute proxyPtr attributeName haskellAttributeDataPtr)
      haskellAttributeData <- peek haskellAttributeDataPtr
      case tangoAttributeData haskellAttributeData of
        HaskellAttributeDataStringArray (HaskellTangoVarArray {varArrayValues}) -> do
          firstString <- peek varArrayValues
          result <- pack <$> peekCString firstString
          tango_free_AttributeData haskellAttributeDataPtr
          pure result
        _ -> do
          tango_free_AttributeData haskellAttributeDataPtr
          error "invalid type of attribute, not a string"

readStateAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> m HaskellTangoDevState
readStateAttribute proxyPtr =
  liftIO $ withCString "State" $ \attributeName -> do
    alloca $ \haskellAttributeDataPtr -> do
      checkResult (tango_read_attribute proxyPtr attributeName haskellAttributeDataPtr)
      haskellAttributeData <- peek haskellAttributeDataPtr
      case tangoAttributeData haskellAttributeData of
        HaskellAttributeDataStateArray (HaskellTangoVarArray {varArrayValues}) -> do
          firstValue <- peek varArrayValues
          tango_free_AttributeData haskellAttributeDataPtr
          pure firstValue
        _ -> do
          tango_free_AttributeData haskellAttributeDataPtr
          error "invalid type of attribute, not a state"

readLong64Attribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> m Int
readLong64Attribute proxyPtr attributeNameHaskell =
  -- FIXME: This is 99% the same as readStringAttribute!
  liftIO $ withCString (unpack attributeNameHaskell) $ \attributeName -> do
    alloca $ \haskellAttributeDataPtr -> do
      checkResult (tango_read_attribute proxyPtr attributeName haskellAttributeDataPtr)
      haskellAttributeData <- peek haskellAttributeDataPtr
      case tangoAttributeData haskellAttributeData of
        HaskellAttributeDataLong64Array (HaskellTangoVarArray {varArrayValues}) -> do
          firstLong <- peek varArrayValues
          tango_free_AttributeData haskellAttributeDataPtr
          pure (fromIntegral firstLong)
        _ -> do
          tango_free_AttributeData haskellAttributeDataPtr
          error "invalid type of attribute, not an int"

readUShortAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> m Int
readUShortAttribute proxyPtr attributeNameHaskell =
  -- FIXME: This is 99% the same as readStringAttribute!
  liftIO $ withCString (unpack attributeNameHaskell) $ \attributeName -> do
    alloca $ \haskellAttributeDataPtr -> do
      checkResult (tango_read_attribute proxyPtr attributeName haskellAttributeDataPtr)
      haskellAttributeData <- peek haskellAttributeDataPtr
      case tangoAttributeData haskellAttributeData of
        HaskellAttributeDataUShortArray (HaskellTangoVarArray {varArrayValues}) -> do
          firstValue <- peek varArrayValues
          tango_free_AttributeData haskellAttributeDataPtr
          pure (fromIntegral firstValue)
        _ -> do
          tango_free_AttributeData haskellAttributeDataPtr
          error "invalid type of attribute, not an ushort"

readDoubleAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> m Double
readDoubleAttribute proxyPtr attributeNameHaskell =
  -- FIXME: This is 99% the same as readStringAttribute!
  liftIO $ withCString (unpack attributeNameHaskell) $ \attributeName -> do
    alloca $ \haskellAttributeDataPtr -> do
      checkResult (tango_read_attribute proxyPtr attributeName haskellAttributeDataPtr)
      haskellAttributeData <- peek haskellAttributeDataPtr
      case tangoAttributeData haskellAttributeData of
        HaskellAttributeDataDoubleArray (HaskellTangoVarArray {varArrayValues}) -> do
          firstValue <- peek varArrayValues
          tango_free_AttributeData haskellAttributeDataPtr
          pure (realToFrac firstValue)
        _ -> do
          tango_free_AttributeData haskellAttributeDataPtr
          error "invalid type of attribute, not a double"

readBoolAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> m Bool
readBoolAttribute proxyPtr attributeNameHaskell =
  -- FIXME: This is 99% the same as readStringAttribute!
  liftIO $ withCString (unpack attributeNameHaskell) $ \attributeName -> do
    alloca $ \haskellAttributeDataPtr -> do
      checkResult (tango_read_attribute proxyPtr attributeName haskellAttributeDataPtr)
      haskellAttributeData <- peek haskellAttributeDataPtr
      case tangoAttributeData haskellAttributeData of
        HaskellAttributeDataBoolArray (HaskellTangoVarArray {varArrayValues}) -> do
          firstValue <- peek varArrayValues
          tango_free_AttributeData haskellAttributeDataPtr
          pure (firstValue == 0)
        _ -> do
          tango_free_AttributeData haskellAttributeDataPtr
          error "invalid type of attribute, not a bool"

commandInOutVoid :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> m ()
commandInOutVoid proxyPtr commandName =
  liftIO $
    withCString (unpack commandName) $
      \commandNamePtr ->
        with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \commandDataInPtr -> with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \commandDataOutPtr ->
          checkResult $ tango_command_inout proxyPtr commandNamePtr commandDataInPtr commandDataOutPtr

newtype PropertyName = PropertyName {getPropertyName :: Text}

data InitedServer = InitedServer

newtype ServerStatus = ServerStatus Text

withCStringFromText :: (UnliftIO.MonadUnliftIO m) => Text -> (CString -> m a) -> m a
withCStringFromText t = withCString (unpack t)

newtype CommandName = CommandName Text

data TangoServerCommand = ServerCommandVoidVoid CommandName (DeviceInstancePtr -> IO ())

newtype AttributeName = AttributeName {getAttributeName :: Text}

data TangoServerAttributeAccessor a = TangoServerAttributeAccessor (DeviceInstancePtr -> IO a) (Maybe (DeviceInstancePtr -> a -> IO ()))

writeTypeFromAccessor :: TangoServerAttributeAccessor a -> HaskellAttrWriteType
writeTypeFromAccessor (TangoServerAttributeAccessor _ Nothing) = Read
writeTypeFromAccessor _ = ReadWrite

newtype TangoServerAttributeTypes = TangoServerAttributeTypeString (TangoServerAttributeAccessor Text)

data TangoServerAttribute = TangoServerAttribute
  { tangoServerAttributeName :: AttributeName,
    tangoServerAttributeAccessor :: TangoServerAttributeTypes
  }

tangoServerInit ::
  forall m.
  (UnliftIO.MonadUnliftIO m) =>
  [PropertyName] ->
  ServerStatus ->
  HaskellTangoDevState ->
  [TangoServerAttribute] ->
  [TangoServerCommand] ->
  DeviceInitCallback ->
  m (Either Text InitedServer)
tangoServerInit propertyNames (ServerStatus initialStatus) initialState attributes commands deviceInitCallback = do
  progName <- getProgName
  args <- getArgs
  freeFinalizerWrapped <- liftIO (createGlobalFinalizer free)
  deviceInitCallbackWrapped <- liftIO (createDeviceInitCallback deviceInitCallback)
  let voidWrapped :: (DeviceInstancePtr -> IO ()) -> CommandCallback
      voidWrapped voidFunction instance' _ptrToBeIgnored = do
        voidFunction instance'
        pure nullPtr
      extractCommandName :: TangoServerCommand -> Text
      extractCommandName (ServerCommandVoidVoid (CommandName n) _) = n
      extractAttributeName :: TangoServerAttribute -> Text
      extractAttributeName = getAttributeName . tangoServerAttributeName
      wrapCommand :: TangoServerCommand -> CommandCallback
      wrapCommand (ServerCommandVoidVoid _name f) = voidWrapped f
      extractAttributeDataType :: TangoServerAttribute -> HaskellTangoDataType
      extractAttributeDataType (TangoServerAttribute {tangoServerAttributeAccessor = TangoServerAttributeTypeString _}) = HaskellDevString
      extractWriteType :: TangoServerAttribute -> HaskellAttrWriteType
      extractWriteType (TangoServerAttribute {tangoServerAttributeAccessor = TangoServerAttributeTypeString accessor}) = writeTypeFromAccessor accessor
      withConvertedCommand :: TangoServerCommand -> (Ptr HaskellCommandDefinition -> m ()) -> m ()
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
      extractGetCallback :: TangoServerAttribute -> IO (FunPtr HaskellAttributeGetter)
      extractGetCallback (TangoServerAttribute {tangoServerAttributeAccessor = TangoServerAttributeTypeString (TangoServerAttributeAccessor getter _)}) =
        createGetterWrapper \devicePtr writePtr -> do
          textValue <- getter devicePtr
          cstringValue <- newCString (unpack textValue)
          poke (castPtr writePtr) cstringValue
      extractSetCallback :: TangoServerAttribute -> IO (FunPtr HaskellAttributeSetter)
      extractSetCallback (TangoServerAttribute {tangoServerAttributeAccessor = TangoServerAttributeTypeString (TangoServerAttributeAccessor _ (Just setter))}) =
        createSetterWrapper \devicePtr readPtr -> do
          readCstring <- peekCString (castPtr readPtr)
          setter devicePtr (pack readCstring)
      extractSetCallback (TangoServerAttribute {tangoServerAttributeAccessor = TangoServerAttributeTypeString (TangoServerAttributeAccessor _ Nothing)}) =
        createSetterWrapper \_devicePtr _readPtr -> pure ()
      withConvertedAttribute :: TangoServerAttribute -> (Ptr HaskellAttributeDefinition -> m ()) -> m ()
      withConvertedAttribute tsa f = do
        withCString (unpack (extractAttributeName tsa)) \attributeNameC -> do
          setCallbackWrapper <- liftIO (extractSetCallback tsa)
          getCallbackWrapper <- liftIO (extractGetCallback tsa)
          with
            ( HaskellAttributeDefinition
                attributeNameC
                (extractAttributeDataType tsa)
                (extractWriteType tsa)
                setCallbackWrapper
                getCallbackWrapper
            )
            f

  forM_ commands \haskellCommand -> withConvertedCommand haskellCommand (liftIO . tango_server_add_command_definition)
  forM_ attributes \haskellAttribute -> withConvertedAttribute haskellAttribute (liftIO . tango_server_add_attribute_definition)
  case args of
    [] -> pure (Left "cannot initialize device server, missing first argument (instance name)")
    (instanceName : _) -> do
      liftIO $ putStrLn $ "initializing server with class name " <> pack progName <> ", instance name " <> pack instanceName
      withCString progName \progNameC' ->
        withCString instanceName \instanceNameC' ->
          withArray [progNameC', instanceNameC'] \programArgumentsC ->
            withCString (unpack initialStatus) \initialStatusC -> do
              mapM_
                (\propName -> withCString propName (liftIO . tango_server_add_property))
                (unpack . getPropertyName <$> propertyNames)
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
  resultAsCString <- withCStringFromText n (liftIO . tango_server_read_property instance')
  resultAsString <- peekCString resultAsCString
  pure (strip (pack resultAsString))
