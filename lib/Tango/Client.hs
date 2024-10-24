{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Description : High-level interface to all client-related functions (mostly functions using a Device Proxy)
--
-- = General Notes
-- == Strictness
--
-- Record values are generally /strict/.
--
-- == Haskell Types
--
-- We're not using and C types when it would be user-facing. Texts are
-- encoded/decoded as 'Data.Text'. Numeric types are converted to
-- 'Int', unless it's about actual payload data (attributes and
-- commands), where the appropriately sized types are used.
--
-- Generally speaking, we convert /spectrum/ types to /Haskell lists/
-- (a vector would have been an option, and maybe we add that
-- possibility, too, if the need arises) and /image/ types to the
-- 'Image' type which, again, uses lists intenally.
--
-- == IO
--
-- The higher-level functions in this module are in 'MonadIO' instead
-- of just 'IO' so you can easily use them in your monad transformer
-- stacks.
--
-- == Errors
--
-- Errors are thrown as exceptions of type 'TangoException'. User errors (such as reading a string attribute with a "read int" function) are thrown via 'error' instead.
--
-- == Properties
--
-- The property retrieval API for Tango is elaborate, supporting different data types. We condensed this down to
-- retrieving lists of strings. Conversion needs to happen on the Haskell side for now.
--
-- = Examples
--
-- == Reading and writing a scalar, boolean attribute
--
-- >{-# LANGUAGE BlockArguments #-}
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >module Main where
-- >
-- >import Tango.Client
-- >
-- >main =
-- >  case parseTangoUrl "sys/tg_test/1" of
-- >    Left e -> error "couldn't resolve tango URL"
-- >    Right deviceAddress -> withDeviceProxy deviceAddress \proxy -> do
-- >      booleanResult <- readBoolAttribute proxy (AttributeName "boolean_scalar")
-- >      putStrLn $ "boolean_scalar is " <> show (tangoValueRead booleanResult)
-- >
-- >      writeBoolAttribute proxy (AttributeName "boolean_scalar") True
--
-- == Reading a spectrum string attribute
--
-- >{-# LANGUAGE BlockArguments #-}
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >module Main where
-- >
-- >import Tango.Client
-- >import qualified Data.Text.IO as TIO
-- >
-- >main =
-- >  case parseTangoUrl "sys/tg_test/1" of
-- >    Left e -> error "couldn't resolve tango URL"
-- >    Right deviceAddress -> withDeviceProxy deviceAddress \proxy -> do
-- >      result <- readBoolSpectrumAttribute proxy (AttributeName "string_spectrum_ro")
-- >      mapM_ TIO.putStrLn result
module Tango.Client
  ( -- * Basics and initialization

    --

    -- | To ensure proper cleanup, you should prefer the 'withDeviceProxy' function to initialize a proxy to a device, and then do something with it.
    DeviceProxy,
    TangoUrl,
    Milliseconds (Milliseconds),
    parseTangoUrl,
    withDeviceProxy,
    newDeviceProxy,
    deleteDeviceProxy,
    TangoException (TangoException),
    RawCommon.ErrSeverity (..),
    DevFailed (DevFailed),
    devFailedDesc,
    devFailedReason,
    devFailedOrigin,
    devFailedSeverity,

    -- * Attributes
    AttributeName (AttributeName),
    AttributeInfo (AttributeInfo),
    getConfigsForAttributes,
    getConfigForAttribute,
    TangoValue (TangoValue, tangoValueRead, tangoValueWrite),
    Image (Image, imageContent, imageDimX, imageDimY),
    TangoAttrMemorizedType (NotKnown, None, Memorized, MemorizedWriteInit),

    -- ** More general types

    -- *** Reading
    readIntegralAttribute,
    readIntegralImageAttribute,
    readIntegralSpectrumAttribute,
    readRealAttribute,
    readRealImageAttribute,
    readRealSpectrumAttribute,

    -- *** Writing
    writeIntegralAttribute,
    writeIntegralImageAttribute,
    writeIntegralSpectrumAttribute,
    writeRealAttribute,
    writeRealImageAttribute,
    writeRealSpectrumAttribute,

    -- ** Specific types

    -- *** Reading
    readBoolAttribute,
    readBoolImageAttribute,
    readBoolSpectrumAttribute,
    readDoubleAttribute,
    readDoubleImageAttribute,
    readDoubleSpectrumAttribute,
    readEnumAttribute,
    readEnumImageAttribute,
    readEnumSpectrumAttribute,
    readFloatAttribute,
    readFloatImageAttribute,
    readFloatSpectrumAttribute,
    readLong64Attribute,
    readLong64ImageAttribute,
    readLong64SpectrumAttribute,
    readLongAttribute,
    readLongImageAttribute,
    readLongSpectrumAttribute,
    readShortAttribute,
    readShortImageAttribute,
    readShortSpectrumAttribute,
    readStateAttribute,
    readStateImageAttribute,
    readStateSpectrumAttribute,
    readStringAttribute,
    readStringImageAttribute,
    readStringSpectrumAttribute,
    readULong64Attribute,
    readULong64ImageAttribute,
    readULong64SpectrumAttribute,
    readULongAttribute,
    readULongImageAttribute,
    readULongSpectrumAttribute,
    readUShortAttribute,
    readUShortImageAttribute,
    readUShortSpectrumAttribute,

    -- *** Writing
    writeBoolAttribute,
    writeBoolImageAttribute,
    writeBoolSpectrumAttribute,
    writeDoubleAttribute,
    writeDoubleImageAttribute,
    writeDoubleSpectrumAttribute,
    writeEnumAttribute,
    writeEnumImageAttribute,
    writeEnumSpectrumAttribute,
    writeFloatAttribute,
    writeFloatImageAttribute,
    writeFloatSpectrumAttribute,
    writeLong64Attribute,
    writeLong64ImageAttribute,
    writeLong64SpectrumAttribute,
    writeLongAttribute,
    writeLongImageAttribute,
    writeLongSpectrumAttribute,
    writeShortAttribute,
    writeShortImageAttribute,
    writeShortSpectrumAttribute,
    writeStateAttribute,
    writeStateImageAttribute,
    writeStateSpectrumAttribute,
    writeStringAttribute,
    writeStringImageAttribute,
    writeStringSpectrumAttribute,
    writeULong64Attribute,
    writeULong64ImageAttribute,
    writeULong64SpectrumAttribute,
    writeULongAttribute,
    writeULongImageAttribute,
    writeULongSpectrumAttribute,
    writeUShortAttribute,
    writeUShortImageAttribute,
    writeUShortSpectrumAttribute,

    -- * Commands
    CommandName (CommandName),
    DisplayLevel (..),
    commandInVoidOutVoid,
    CommandData (..),
    commandInOutGeneric,
    commandInEnumOutGeneric,
    commandInGenericOutEnum,
    commandInEnumOutEnum,
    commandListQuery,
    commandQuery,
    CommandInfo (..),

    -- * Properties
    Property (..),
    PropertyName (..),
    getDeviceProperties,
    putDeviceProperties,
    deleteDeviceProperties,
    HaskellTangoDevState (Alarm, Close, Disable, Extract, Fault, Init, Insert, Moving, Off, On, Open, Running, Standby, Unknown),

    -- * Events
    subscribeEvent,
    unsubscribeEvent,
    withSubscribedEvent,
    SubscribedEvent,
    EventType (..),

    -- * Database proxy
    DatabaseProxy,
    createDatabaseProxy,
    deleteDatabaseProxy,
    withDatabaseProxy,
    databaseSearchByDeviceName,
    databaseSearchByClass,
    databaseSearchObjectsByName,
    databaseSearchObjectPropertiesByName,

    -- * Various other device-related functions
    lockDevice,
    unlockDevice,
    getAttributeNames,
    withLocked,
    setTimeout,
    getTimeout,
    pollCommand,
    stopPollCommand,
    pollAttribute,
    stopPollAttribute,
  )
where

import Control.Applicative (Applicative, pure, (<*>))
import Control.Exception (Exception, throw)
import Control.Monad (void, when, (>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool (False, True), otherwise, (||))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq, (/=))
import Data.Foldable (any)
import Data.Function (const, id, ($), (.))
import Data.Functor (Functor, (<$>))
import Data.Int (Int, Int16, Int32, Int64)
import Data.List (drop, head, length, splitAt)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord ((>))
import Data.Semigroup ((<>))
import Data.Text (Text, intercalate, isPrefixOf, null, pack, splitOn, unpack)
import Data.Traversable (traverse)
import Data.Word (Word16, Word64)
import Foreign (Storable)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (IO)
import Tango.Raw.Common
  ( DatabaseProxyPtr,
    DevFailed (DevFailed, devFailedDesc, devFailedOrigin, devFailedReason, devFailedSeverity),
    DeviceProxyPtr,
    EventType,
    HaskellAttrWriteType,
    HaskellAttributeData (HaskellAttributeData, dataFormat, dataQuality, dataType, dimX, dimY, name, nbRead, tangoAttributeData, timeStamp),
    HaskellAttributeInfoList (HaskellAttributeInfoList, attributeInfoListLength, attributeInfoListSequence),
    HaskellCommandData (HaskellCommandData, tangoCommandData),
    HaskellCommandInfo (HaskellCommandInfo, cmdDisplayLevel, cmdInType, cmdInTypeDesc, cmdName, cmdOutType, cmdOutTypeDesc, cmdTag),
    HaskellCommandInfoList (HaskellCommandInfoList, commandInfoLength, commandInfoSequence),
    HaskellDataFormat (HaskellImage, HaskellScalar, HaskellSpectrum),
    HaskellDataQuality (HaskellValid),
    HaskellDbData (HaskellDbData, dbDataLength, dbDataSequence),
    HaskellDbDatum (HaskellDbDatum, dbDatumIsEmpty, dbDatumPropData, dbDatumPropertyName, dbDatumWrongDataType),
    HaskellDispLevel,
    HaskellErrorStack (errorStackLength, errorStackSequence),
    HaskellTangoAttributeData
      ( HaskellAttributeDataBoolArray,
        HaskellAttributeDataDoubleArray,
        HaskellAttributeDataFloatArray,
        HaskellAttributeDataLong64Array,
        HaskellAttributeDataLongArray,
        HaskellAttributeDataShortArray,
        HaskellAttributeDataStateArray,
        HaskellAttributeDataStringArray,
        HaskellAttributeDataULong64Array,
        HaskellAttributeDataULongArray,
        HaskellAttributeDataUShortArray
      ),
    HaskellTangoCommandData (HaskellCommandBool, HaskellCommandCString, HaskellCommandDevEnum, HaskellCommandDevState, HaskellCommandDouble, HaskellCommandFloat, HaskellCommandInt32, HaskellCommandLong64, HaskellCommandShort, HaskellCommandULong64, HaskellCommandUShort, HaskellCommandVarBool, HaskellCommandVarCString, HaskellCommandVarDevState, HaskellCommandVarDouble, HaskellCommandVarFloat, HaskellCommandVarLong, HaskellCommandVarLong64, HaskellCommandVarShort, HaskellCommandVarULong, HaskellCommandVarULong64, HaskellCommandVarUShort, HaskellCommandVoid),
    HaskellTangoDataType (HaskellDevBoolean, HaskellDevDouble, HaskellDevEnum, HaskellDevFloat, HaskellDevInt, HaskellDevLong, HaskellDevLong64, HaskellDevShort, HaskellDevState, HaskellDevString, HaskellDevULong, HaskellDevULong64, HaskellDevUShort, HaskellDevVarBooleanArray, HaskellDevVarDoubleArray, HaskellDevVarFloatArray, HaskellDevVarLong64Array, HaskellDevVarLongArray, HaskellDevVarShortArray, HaskellDevVarStateArray, HaskellDevVarStringArray, HaskellDevVarULong64Array, HaskellDevVarULongArray, HaskellDevVarUShortArray, HaskellDevVoid),
    HaskellTangoDevState (Alarm, Close, Disable, Extract, Fault, Init, Insert, Moving, Off, On, Open, Running, Standby, Unknown),
    HaskellTangoPropertyData (HaskellPropStringArray),
    HaskellTangoVarArray (HaskellTangoVarArray, varArrayLength, varArrayValues),
    TangoAttrMemorizedType (Memorized, MemorizedWriteInit, None, NotKnown),
    Timeval (Timeval),
    createEventCallbackWrapper,
    tango_command_inout,
    tango_command_list_query,
    tango_command_query,
    tango_create_database_proxy,
    tango_create_device_proxy,
    tango_create_event_callback,
    tango_delete_database_proxy,
    tango_delete_device_property,
    tango_delete_device_proxy,
    tango_free_AttributeData,
    tango_free_AttributeInfoList,
    tango_free_CommandInfo,
    tango_free_CommandInfoList,
    tango_free_DbDatum,
    tango_free_VarStringArray,
    tango_free_event_callback,
    tango_get_attribute_config,
    tango_get_attribute_list,
    tango_get_device_exported,
    tango_get_device_exported_for_class,
    tango_get_device_property,
    tango_get_object_list,
    tango_get_object_property_list,
    tango_get_timeout_millis,
    tango_lock,
    tango_poll_attribute,
    tango_poll_command,
    tango_put_device_property,
    tango_read_attribute,
    tango_set_timeout_millis,
    tango_stop_poll_attribute,
    tango_stop_poll_command,
    tango_subscribe_event,
    tango_unlock,
    tango_unsubscribe_event,
    tango_write_attribute,
  )
import qualified Tango.Raw.Common as RawCommon
import Text.Show (Show, show)
import UnliftIO (MonadUnliftIO, bracket, finally, withRunInIO)
import UnliftIO.Foreign (CBool, CDouble, CFloat, CLong, CShort, CULong, CUShort, alloca, free, new, newArray, newCString, peek, peekArray, peekCString, with, withArray, withCString)
import Prelude (Bounded, Double, Enum (fromEnum, toEnum), Float, Fractional, Integral, Num ((*)), Real, error, fromIntegral, realToFrac)

-- | This wraps the Tango exception trace in Haskell
newtype TangoException = TangoException [DevFailed Text] deriving (Show)

instance Exception TangoException

withCStringText :: (MonadUnliftIO m) => Text -> (CString -> m a) -> m a
withCStringText t = withCString (unpack t)

peekCStringText :: (MonadUnliftIO m) => CString -> m Text
peekCStringText x = do
  result <- liftIO (peekCString x)
  pure (pack result)

peekCStringArrayText :: (MonadUnliftIO m, Integral i) => i -> Ptr CString -> m [Text]
peekCStringArrayText len x = do
  ptrList <- liftIO $ peekArray (fromIntegral len) x
  traverse peekCStringText ptrList

-- | Execute a Tango action that potentially returns an error; convert this error into 'TangoException'
checkResult :: (MonadUnliftIO m) => m (Ptr HaskellErrorStack) -> m ()
checkResult action = do
  es <- action
  when (es /= nullPtr) $ do
    errorStack <- liftIO $ peek es
    stackItems <- peekArray (fromIntegral (errorStackLength errorStack)) (errorStackSequence errorStack)
    formattedStackItems :: [DevFailed Text] <- traverse (traverse peekCStringText) stackItems
    throw (TangoException formattedStackItems)

-- | Newtype wrapper around a Tango URL like @tango:\/\/host:port\/foo\/bar\/baz@. Retrieve via 'parseTangoUrl'
newtype TangoUrl = TangoUrl Text

-- | Try to parse a Tango URL like @tango:\/\/host:port\/foo\/bar\/baz@ (the left side of the @Either@ will be an error message)
parseTangoUrl :: Text -> Either Text TangoUrl
parseTangoUrl url =
  let tangoUrlFromText' url' =
        let urlComponents = splitOn "/" url'
         in if length urlComponents /= 3 || any null urlComponents
              then Left $ "\"" <> url <> "\" is not a valid tango URL: has to be of the form \"[tango://host:port/]domain/family/member\""
              else Right (TangoUrl url)
   in tangoUrlFromText' $
        if "tango://" `isPrefixOf` url
          then intercalate "/" $ drop 3 (splitOn "/" url)
          else url

-- | Wraps a pointer to a device proxy
newtype DeviceProxy = DeviceProxy DeviceProxyPtr

-- | This just looks nicer because not a pointer
type DatabaseProxy = DatabaseProxyPtr

boolToCBool :: Bool -> CBool
boolToCBool True = 1
boolToCBool False = 0

cboolToBool :: CBool -> Bool
cboolToBool x
  | x > 0 = True
  | otherwise = False

-- | Create a new device proxy (check 'deleteDeviceProxy' and 'withDeviceProxy', too)
newDeviceProxy :: forall m. (MonadUnliftIO m) => TangoUrl -> m DeviceProxy
newDeviceProxy (TangoUrl url) = liftIO $
  alloca $ \proxyPtrPtr -> do
    withCString (unpack url) $ \proxyName -> do
      checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
      DeviceProxy <$> peek proxyPtrPtr

-- | Delete a device proxy (check 'newDeviceProxy' and 'withDeviceProxy', too)
deleteDeviceProxy :: forall m. (MonadUnliftIO m) => DeviceProxy -> m ()
deleteDeviceProxy (DeviceProxy proxyPtr) = liftIO $ checkResult (tango_delete_device_proxy proxyPtr)

-- | Safely initialize and clean up a device proxy for a given tango URL
withDeviceProxy :: forall m a. (MonadUnliftIO m) => TangoUrl -> (DeviceProxy -> m a) -> m a
withDeviceProxy (TangoUrl proxyAddress) =
  let initialize :: m DeviceProxy
      initialize =
        liftIO $ alloca \proxyPtrPtr -> do
          withCStringText proxyAddress \proxyAddressPtr -> do
            checkResult (tango_create_device_proxy proxyAddressPtr proxyPtrPtr)
            DeviceProxy <$> peek proxyPtrPtr
      deinitialize :: DeviceProxy -> m ()
      deinitialize (DeviceProxy proxyPtrPtr) =
        liftIO $ checkResult (tango_delete_device_proxy proxyPtrPtr)
   in bracket initialize deinitialize

writeScalarAttribute ::
  (MonadUnliftIO m, Storable tangoType) =>
  DeviceProxy ->
  AttributeName ->
  tangoType ->
  HaskellTangoDataType ->
  (HaskellTangoVarArray tangoType -> HaskellTangoAttributeData) ->
  m ()
writeScalarAttribute (DeviceProxy proxyPtr) (AttributeName attributeName) newValue tangoType intract = do
  withCStringText attributeName $ \attributeNamePtr ->
    with newValue $ \newValuePtr ->
      with
        ( HaskellAttributeData
            { dataFormat = HaskellScalar,
              dataQuality = HaskellValid,
              nbRead = 0,
              name = attributeNamePtr,
              dimX = 1,
              dimY = 1,
              timeStamp = Timeval 0 0,
              dataType = tangoType,
              tangoAttributeData = intract (HaskellTangoVarArray 1 newValuePtr)
            }
        )
        (liftIO . void . tango_write_attribute proxyPtr)

writeSpectrumAttribute ::
  (MonadUnliftIO m, Storable tangoType) =>
  DeviceProxy ->
  AttributeName ->
  [tangoType] ->
  HaskellTangoDataType ->
  (HaskellTangoVarArray tangoType -> HaskellTangoAttributeData) ->
  m ()
writeSpectrumAttribute (DeviceProxy proxyPtr) (AttributeName attributeName) newValues tangoType intract =
  withCStringText attributeName $ \attributeNamePtr ->
    withArray newValues \newValuesPtr ->
      with
        ( HaskellAttributeData
            { dataFormat = HaskellSpectrum,
              dataQuality = HaskellValid,
              nbRead = 0,
              name = attributeNamePtr,
              dimX = fromIntegral (length newValues),
              dimY = 1,
              timeStamp = Timeval 0 0,
              dataType = tangoType,
              tangoAttributeData = intract (HaskellTangoVarArray (fromIntegral (length newValues)) newValuesPtr)
            }
        )
        (liftIO . void . tango_write_attribute proxyPtr)

writeImageAttribute ::
  (MonadUnliftIO m, Storable tangoType) =>
  DeviceProxy ->
  AttributeName ->
  Image tangoType ->
  HaskellTangoDataType ->
  (HaskellTangoVarArray tangoType -> HaskellTangoAttributeData) ->
  m ()
writeImageAttribute (DeviceProxy proxyPtr) (AttributeName attributeName) newImage tangoType intract =
  withCStringText attributeName $ \attributeNamePtr ->
    withArray (imageContent newImage) \newValuesPtr ->
      with
        ( HaskellAttributeData
            { dataFormat = HaskellImage,
              dataQuality = HaskellValid,
              nbRead = 0,
              name = attributeNamePtr,
              dimX = fromIntegral (imageDimX newImage),
              dimY = fromIntegral (imageDimY newImage),
              timeStamp = Timeval 0 0,
              dataType = tangoType,
              tangoAttributeData = intract (HaskellTangoVarArray (fromIntegral (length (imageContent newImage))) newValuesPtr)
            }
        )
        (liftIO . void . tango_write_attribute proxyPtr)

-- | Read an attribute irrespective of the concrete integral type. This just uses 'fromIntegral' internally to convert from any integral type. However, we do query the attribute type beforehand, making this two calls instead of just one. If you're really concerned about performance, try to find out the real type of the attribute.
writeIntegralAttribute :: (MonadUnliftIO m, Integral i) => DeviceProxy -> AttributeName -> i -> m ()
writeIntegralAttribute proxy attributeName newValue = do
  config <- getConfigForAttribute proxy attributeName
  case attributeInfoDataType config of
    HaskellDevShort ->
      writeShortAttribute proxy attributeName (fromIntegral newValue)
    HaskellDevUShort ->
      writeUShortAttribute proxy attributeName (fromIntegral newValue)
    HaskellDevLong ->
      writeLongAttribute proxy attributeName (fromIntegral newValue)
    HaskellDevULong ->
      writeULongAttribute proxy attributeName (fromIntegral newValue)
    HaskellDevLong64 ->
      writeLong64Attribute proxy attributeName (fromIntegral newValue)
    HaskellDevULong64 ->
      writeULong64Attribute proxy attributeName (fromIntegral newValue)
    _ -> error $ "tried to write integral attribute " <> show attributeName <> " but the attribute is not an integral type"

-- | Read a spectrum attribute irrespective of the concrete integral type. This just uses 'fromIntegral' internally to convert from any integral type. However, we do query the attribute type beforehand, making this two calls instead of just one. If you're really concerned about performance, try to find out the real type of the attribute.
writeIntegralSpectrumAttribute :: (MonadUnliftIO m, Integral i) => DeviceProxy -> AttributeName -> [i] -> m ()
writeIntegralSpectrumAttribute proxy attributeName newValues = do
  config <- getConfigForAttribute proxy attributeName
  case attributeInfoDataType config of
    HaskellDevVarShortArray ->
      writeShortSpectrumAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarUShortArray ->
      writeUShortSpectrumAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarLongArray ->
      writeLongSpectrumAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarULongArray ->
      writeULongSpectrumAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarLong64Array ->
      writeLong64SpectrumAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarULong64Array ->
      writeULong64SpectrumAttribute proxy attributeName (fromIntegral <$> newValues)
    _ -> error $ "tried to write integral attribute " <> show attributeName <> " but the attribute is not an integral type"

-- | Read a spectrum attribute irrespective of the concrete integral type. This just uses 'fromIntegral' internally to convert from any integral type. However, we do query the attribute type beforehand, making this two calls instead of just one. If you're really concerned about performance, try to find out the real type of the attribute.
writeIntegralImageAttribute :: (MonadUnliftIO m, Integral i) => DeviceProxy -> AttributeName -> Image i -> m ()
writeIntegralImageAttribute proxy attributeName newValues = do
  config <- getConfigForAttribute proxy attributeName
  case attributeInfoDataType config of
    HaskellDevVarShortArray ->
      writeShortImageAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarUShortArray ->
      writeUShortImageAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarLongArray ->
      writeLongImageAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarULongArray ->
      writeULongImageAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarLong64Array ->
      writeLong64ImageAttribute proxy attributeName (fromIntegral <$> newValues)
    HaskellDevVarULong64Array ->
      writeULong64ImageAttribute proxy attributeName (fromIntegral <$> newValues)
    _ -> error $ "tried to write integral attribute " <> show attributeName <> " but the attribute is not an integral type"

-- | Read a spectrum attribute irrespective of the concrete integral type. This just uses 'fromIntegral' internally to convert from any integral type. However, we do query the attribute type beforehand, making this two calls instead of just one. If you're really concerned about performance, try to find out the real type of the attribute.
writeRealImageAttribute :: (MonadUnliftIO m, Real i) => DeviceProxy -> AttributeName -> Image i -> m ()
writeRealImageAttribute proxy attributeName newValues = do
  config <- getConfigForAttribute proxy attributeName
  case attributeInfoDataType config of
    HaskellDevFloat ->
      writeFloatImageAttribute proxy attributeName (realToFrac <$> newValues)
    HaskellDevDouble ->
      writeDoubleImageAttribute proxy attributeName (realToFrac <$> newValues)
    _ -> error $ "tried to write real attribute " <> show attributeName <> " but the attribute is not a real type"

-- | Read an attribute irrespective of the concrete integral type. This just uses 'fromIntegral' internally to convert from any integral type. However, we do query the attribute type beforehand, making this two calls instead of just one. If you're really concerned about performance, try to find out the real type of the attribute.
writeRealAttribute :: (MonadUnliftIO m, Real i) => DeviceProxy -> AttributeName -> i -> m ()
writeRealAttribute proxy attributeName newValue = do
  config <- getConfigForAttribute proxy attributeName
  case attributeInfoDataType config of
    HaskellDevFloat ->
      writeFloatAttribute proxy attributeName (realToFrac newValue)
    HaskellDevDouble ->
      writeDoubleAttribute proxy attributeName (realToFrac newValue)
    _ -> error $ "tried to write real attribute " <> show attributeName <> " but the attribute is not a real type"

-- | Read a spectrum attribute irrespective of the concrete real type. This just uses 'realToFrac' internally to convert from any integral type. However, we do query the attribute type beforehand, making this two calls instead of just one. If you're really concerned about performance, try to find out the real type of the attribute.
writeRealSpectrumAttribute :: (MonadUnliftIO m, Real i) => DeviceProxy -> AttributeName -> [i] -> m ()
writeRealSpectrumAttribute proxy attributeName newValues = do
  config <- getConfigForAttribute proxy attributeName
  case attributeInfoDataType config of
    HaskellDevVarFloatArray ->
      writeFloatSpectrumAttribute proxy attributeName (realToFrac <$> newValues)
    HaskellDevVarDoubleArray ->
      writeDoubleSpectrumAttribute proxy attributeName (realToFrac <$> newValues)
    _ -> error $ "tried to write real attribute " <> show attributeName <> " but the attribute is not a real type"

-- | Write a boolean scalar attribute
writeBoolAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Bool -> m ()
writeBoolAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (boolToCBool newValue) HaskellDevBoolean HaskellAttributeDataBoolArray

-- | Write a boolean spectrum attribute
writeBoolSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Bool] -> m ()
writeBoolSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (boolToCBool <$> newValues) HaskellDevBoolean HaskellAttributeDataBoolArray

-- | Write a boolean image attribute
writeBoolImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Bool -> m ()
writeBoolImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (boolToCBool <$> newImage) HaskellDevBoolean HaskellAttributeDataBoolArray

-- | Write a short scalar attribute
writeShortAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Int16 -> m ()
writeShortAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevShort HaskellAttributeDataShortArray

-- | Write a short spectrum attribute
writeShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Int16] -> m ()
writeShortSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevShort HaskellAttributeDataShortArray

-- | Write a short image attribute
writeShortImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Int16 -> m ()
writeShortImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevShort HaskellAttributeDataShortArray

-- | Write an unsigned short scalar attribute
writeUShortAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Word16 -> m ()
writeUShortAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevUShort HaskellAttributeDataUShortArray

-- | Write an unsigned short spectrum attribute
writeUShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Word16] -> m ()
writeUShortSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevUShort HaskellAttributeDataUShortArray

-- | Write an unsigned short image attribute
writeUShortImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Word16 -> m ()
writeUShortImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevUShort HaskellAttributeDataUShortArray

-- | Write a long scalar attribute
writeLongAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Int64 -> m ()
writeLongAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevLong HaskellAttributeDataLongArray

-- | Write a long spectrum attribute
writeLongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Int64] -> m ()
writeLongSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevLong HaskellAttributeDataLongArray

-- | Write a long image attribute
writeLongImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Int64 -> m ()
writeLongImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevLong HaskellAttributeDataLongArray

-- | Write an unsigned long scalar attribute
writeULongAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Word64 -> m ()
writeULongAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevULong HaskellAttributeDataULongArray

-- | Write an unsigned long spectrum attribute
writeULongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Word64] -> m ()
writeULongSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevULong HaskellAttributeDataULongArray

-- | Write an unsigned long image attribute
writeULongImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Word64 -> m ()
writeULongImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevULong HaskellAttributeDataULongArray

-- | Write an unsigned long64 scalar attribute
writeULong64Attribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Word64 -> m ()
writeULong64Attribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevULong64 HaskellAttributeDataULong64Array

-- | Write an unsigned long64 spectrum attribute
writeULong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Word64] -> m ()
writeULong64SpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevULong64 HaskellAttributeDataULong64Array

-- | Write an unsigned long64 image attribute
writeULong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Word64 -> m ()
writeULong64ImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevULong64 HaskellAttributeDataULong64Array

-- | Write a float scalar attribute
writeFloatAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Double -> m ()
writeFloatAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (realToFrac newValue) HaskellDevFloat HaskellAttributeDataFloatArray

-- | Write a float spectrum attribute
writeFloatSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Double] -> m ()
writeFloatSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (realToFrac <$> newValues) HaskellDevFloat HaskellAttributeDataFloatArray

-- | Write a float image attribute
writeFloatImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Double -> m ()
writeFloatImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (realToFrac <$> newImage) HaskellDevFloat HaskellAttributeDataFloatArray

-- | Write a double scalar attribute
writeDoubleAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Double -> m ()
writeDoubleAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (realToFrac newValue) HaskellDevDouble HaskellAttributeDataDoubleArray

-- | Write a double spectrum attribute
writeDoubleSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Double] -> m ()
writeDoubleSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (realToFrac <$> newValues) HaskellDevDouble HaskellAttributeDataDoubleArray

-- | Write a double image attribute
writeDoubleImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Double -> m ()
writeDoubleImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (realToFrac <$> newImage) HaskellDevDouble HaskellAttributeDataDoubleArray

-- | Write a state scalar attribute
writeStateAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> HaskellTangoDevState -> m ()
writeStateAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName newValue HaskellDevState HaskellAttributeDataStateArray

-- | Write a state spectrum attribute
writeStateSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [HaskellTangoDevState] -> m ()
writeStateSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName newValues HaskellDevState HaskellAttributeDataStateArray

-- | Write a state image attribute
writeStateImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image HaskellTangoDevState -> m ()
writeStateImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName newImage HaskellDevState HaskellAttributeDataStateArray

-- | Write an enum scalar attribute
writeEnumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> t -> m ()
writeEnumAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral (fromEnum newValue)) HaskellDevEnum HaskellAttributeDataShortArray

-- | Write an enum spectrum attribute
writeEnumSpectrumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> [t] -> m ()
writeEnumSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral . fromEnum <$> newValues) HaskellDevEnum HaskellAttributeDataShortArray

-- | Write an enum image attribute
writeEnumImageAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> Image t -> m ()
writeEnumImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral . fromEnum <$> newImage) HaskellDevEnum HaskellAttributeDataShortArray

-- | Write a string scalar attribute
writeStringAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Text -> m ()
writeStringAttribute (DeviceProxy proxyPtr) (AttributeName attributeName) newValue =
  withCStringText attributeName \attributeNameC -> do
    withCStringText newValue \newValuePtr ->
      with newValuePtr \newValuePtrPtr ->
        with
          ( HaskellAttributeData
              { dataFormat = HaskellScalar,
                dataQuality = HaskellValid,
                nbRead = 0,
                name = attributeNameC,
                dimX = 1,
                dimY = 1,
                timeStamp = Timeval 0 0,
                dataType = HaskellDevString,
                tangoAttributeData = HaskellAttributeDataStringArray (HaskellTangoVarArray 1 newValuePtrPtr)
              }
          )
          (liftIO . void . tango_write_attribute proxyPtr)

-- | Write a string spectrum attribute
writeStringSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Text] -> m ()
writeStringSpectrumAttribute (DeviceProxy proxyPtr) (AttributeName attributeName) newValues =
  withCStringText attributeName \attributeNameC ->
    bracket (traverse (newCString . unpack) newValues) (traverse free) \stringPointerList ->
      withArray stringPointerList \stringPointerPtr ->
        with
          ( HaskellAttributeData
              { dataFormat = HaskellSpectrum,
                dataQuality = HaskellValid,
                nbRead = 0,
                name = attributeNameC,
                dimX = fromIntegral (length newValues),
                dimY = 1,
                timeStamp = Timeval 0 0,
                dataType = HaskellDevString,
                tangoAttributeData = HaskellAttributeDataStringArray (HaskellTangoVarArray (fromIntegral (length newValues)) stringPointerPtr)
              }
          )
          (liftIO . void . tango_write_attribute proxyPtr)

-- | Write a string image attribute
writeStringImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Text -> m ()
writeStringImageAttribute (DeviceProxy proxyPtr) (AttributeName attributeName) (Image newImage imageX imageY) =
  withCStringText attributeName \attributeNameC ->
    bracket (traverse (newCString . unpack) newImage) (traverse free) \stringPointerList ->
      withArray stringPointerList \stringPointerPtr ->
        with
          ( HaskellAttributeData
              { dataFormat = HaskellSpectrum,
                dataQuality = HaskellValid,
                nbRead = 0,
                name = attributeNameC,
                dimX = fromIntegral imageX,
                dimY = fromIntegral imageY,
                timeStamp = Timeval 0 0,
                dataType = HaskellDevString,
                tangoAttributeData = HaskellAttributeDataStringArray (HaskellTangoVarArray (fromIntegral (imageX * imageY)) stringPointerPtr)
              }
          )
          (liftIO . void . tango_write_attribute proxyPtr)

-- | Write a long64 scalar attribute
writeLong64Attribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Int64 -> m ()
writeLong64Attribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevLong64 HaskellAttributeDataLong64Array

-- | Write a long64 spectrum attribute
writeLong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Int64] -> m ()
writeLong64SpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevLong64 HaskellAttributeDataLong64Array

-- | Write a long64 image attribute
writeLong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Int64 -> m ()
writeLong64ImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevLong64 HaskellAttributeDataLong64Array

-- | Newtype wrapper to wrap an attribute name
newtype AttributeName = AttributeName Text deriving (Show)

-- | Read an attribute's value, call a function on it, and free it up again
withReadAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> CString -> (HaskellAttributeData -> m a) -> m a
withReadAttribute proxyPtr attributeNameC f = alloca \haskellAttributeDataPtr -> do
  liftIO $ checkResult (tango_read_attribute proxyPtr attributeNameC haskellAttributeDataPtr)
  haskellAttributeData <- liftIO $ peek haskellAttributeDataPtr
  finally (f haskellAttributeData) (liftIO $ tango_free_AttributeData haskellAttributeDataPtr)

-- | Read an attribute's value, maybe extract something useful from it, convert that, and free the Tango data up again.
withExtractedAttributeValue :: (MonadUnliftIO m) => (HaskellAttributeData -> m (Maybe a)) -> DeviceProxy -> AttributeName -> (a -> m b) -> m b
withExtractedAttributeValue extractValue (DeviceProxy proxyPtr) (AttributeName attributeNameHaskell) f =
  withCStringText attributeNameHaskell $ \attributeNameC -> withReadAttribute proxyPtr attributeNameC \haskellAttributeData -> do
    extractedValue <- extractValue haskellAttributeData
    case extractedValue of
      Nothing -> error ("invalid type for attribute \"" <> unpack attributeNameHaskell <> "\"")
      Just v -> f v

readAttributeGeneral :: (MonadIO m) => (HaskellAttributeData -> IO (Maybe a)) -> DeviceProxy -> AttributeName -> m a
readAttributeGeneral extractValue (DeviceProxy proxyPtr) (AttributeName attributeNameHaskell) =
  liftIO $ withCStringText attributeNameHaskell $ \attributeName -> do
    alloca $ \haskellAttributeDataPtr -> do
      checkResult (tango_read_attribute proxyPtr attributeName haskellAttributeDataPtr)
      haskellAttributeData <- peek haskellAttributeDataPtr
      extractedValue <- extractValue haskellAttributeData
      case extractedValue of
        Nothing -> error ("invalid type for attribute \"" <> unpack attributeNameHaskell <> "\"")
        Just v ->
          pure v

data AtLeastTwo a = AtLeastTwo a a [a]

-- | Call 'withExtractedAttributeValue' to read an attribute's value (safely), extract the array within, and call a function that makes the parsed contents into something more useful.
readAttributeSimple ::
  (Storable a, Show a, MonadUnliftIO m) =>
  -- | Extract a specific type of data array (usually extracting a single data type like bool, long, ...)
  (HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray a)) ->
  -- | After taking at least two elements from the array given by the previous function, call this function and turn the whole thing into something useful
  (HaskellAttributeData -> AtLeastTwo a -> m b) ->
  DeviceProxy ->
  AttributeName ->
  m b
readAttributeSimple extractValue convertValue proxy attributeName = withExtractedAttributeValue (\d -> pure ((d,) <$> extractValue (tangoAttributeData d))) proxy attributeName \(attributeData, tangoArray) -> do
  arrayElements <- peekArray (fromIntegral (varArrayLength tangoArray)) (varArrayValues tangoArray)
  case arrayElements of
    (first : second : rest) -> convertValue attributeData (AtLeastTwo first second rest)
    _ -> error $ "couldn't read attribute " <> show attributeName <> ": expected a value array of length at least two, but got " <> show arrayElements

readAttributeSimple' ::
  (MonadIO m, Show a) =>
  (HaskellTangoAttributeData -> IO (Maybe [a])) ->
  (HaskellAttributeData -> AtLeastTwo a -> m b) ->
  DeviceProxy ->
  AttributeName ->
  m b
readAttributeSimple' extractValue convertValue proxy attributeName = do
  (attributeData, tangoArray) <- readAttributeGeneral (\d -> ((d,) <$>) <$> extractValue (tangoAttributeData d)) proxy attributeName
  case tangoArray of
    (first : second : rest) -> convertValue attributeData (AtLeastTwo first second rest)
    _ -> error $ "couldn't read attribute " <> show attributeName <> ": expected a value array of length at least two, but got " <> show tangoArray

convertGenericScalar :: (Applicative f) => (a -> b) -> HaskellAttributeData -> AtLeastTwo a -> f (TangoValue b)
convertGenericScalar f _ (AtLeastTwo first second _) = pure (TangoValue (f first) (f second))

convertGenericSpectrum :: (Applicative f) => (a -> b) -> HaskellAttributeData -> AtLeastTwo a -> f (TangoValue [b])
convertGenericSpectrum f (HaskellAttributeData {dimX}) (AtLeastTwo first second remainder) =
  let wholeList = f <$> (first : second : remainder)
      (readValue, writeValue) = splitAt (fromIntegral dimX) wholeList
   in pure (TangoValue readValue writeValue)

convertGenericSpectrum' :: (Applicative f) => HaskellAttributeData -> AtLeastTwo a -> f (TangoValue [a])
convertGenericSpectrum' (HaskellAttributeData {dimX}) (AtLeastTwo first second remainder) =
  let wholeList = first : second : remainder
      (readValue, writeValue) = splitAt (fromIntegral dimX) wholeList
   in pure (TangoValue readValue writeValue)

convertGenericImage :: (Applicative f) => (a1 -> a2) -> HaskellAttributeData -> AtLeastTwo a1 -> f (TangoValue (Image a2))
convertGenericImage f (HaskellAttributeData {dimX, dimY}) (AtLeastTwo first second remainder) =
  let wholeList = f <$> (first : second : remainder)
      (readValue, writeValue) = splitAt (fromIntegral (dimX * dimY)) wholeList
   in pure
        ( TangoValue
            (Image readValue (fromIntegral dimX) (fromIntegral dimY))
            (Image writeValue (fromIntegral dimX) (fromIntegral dimY))
        )

convertGenericImage' :: (Applicative f) => HaskellAttributeData -> AtLeastTwo a1 -> f (TangoValue (Image a1))
convertGenericImage' (HaskellAttributeData {dimX, dimY}) (AtLeastTwo first second remainder) =
  let wholeList = first : second : remainder
      (readValue, writeValue) = splitAt (fromIntegral (dimX * dimY)) wholeList
   in pure
        ( TangoValue
            (Image readValue (fromIntegral dimX) (fromIntegral dimY))
            (Image writeValue (fromIntegral dimX) (fromIntegral dimY))
        )

-- | Represents an attribute's value, with read and write part, for different data types. Fields for quality etc. are currently missing
data TangoValue a = TangoValue
  { -- | Read part of the attribute's value
    tangoValueRead :: a,
    -- | Write part of the attribute's value
    tangoValueWrite :: a
  }
  deriving (Show)

-- | Represents an image attribute's value
data Image a = Image
  { -- | Image pixels
    imageContent :: ![a],
    -- | X dimension of the image
    imageDimX :: !Int,
    -- | Y dimension of the image
    imageDimY :: !Int
  }
  deriving (Show, Functor)

-- | Read an attribute irrespective of the concrete integral type. This just uses 'fromIntegral' internally.
readIntegralAttribute :: forall m i. (MonadUnliftIO m, Integral i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue i)
readIntegralAttribute = readAttributeSimple' extractIntegral (convertGenericScalar id)

extractIntegral :: (Integral i, MonadUnliftIO m) => HaskellTangoAttributeData -> m (Maybe [i])
extractIntegral (HaskellAttributeDataLongArray a) = do
  arrayElements <- peekArray (fromIntegral (varArrayLength a)) (varArrayValues a)
  pure $ Just (fromIntegral <$> arrayElements)
extractIntegral _ = pure Nothing

-- | Read a spectrum attribute irrespective of the concrete integral element type. This just uses 'fromIntegral' internally.
readIntegralSpectrumAttribute :: (MonadUnliftIO m, Integral i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue [i])
readIntegralSpectrumAttribute = readAttributeSimple' extractIntegral convertGenericSpectrum'

-- | Read a spectrum image attribute irrespective of the concrete integral element type. This just uses 'fromIntegral' internally.
readIntegralImageAttribute :: (MonadUnliftIO m, Integral i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue (Image i))
readIntegralImageAttribute = readAttributeSimple' extractIntegral convertGenericImage'

extractReal :: (Fractional i, MonadUnliftIO m) => HaskellTangoAttributeData -> m (Maybe [i])
extractReal (HaskellAttributeDataDoubleArray a) = do
  arrayElements <- peekArray (fromIntegral (varArrayLength a)) (varArrayValues a)
  pure $ Just (realToFrac <$> arrayElements)
extractReal (HaskellAttributeDataFloatArray a) = do
  arrayElements <- peekArray (fromIntegral (varArrayLength a)) (varArrayValues a)
  pure $ Just (realToFrac <$> arrayElements)
extractReal _ = pure Nothing

-- | Read an attribute irrespective of the concrete real type. This just uses 'realToFrac' internally.
readRealAttribute :: forall m i. (MonadUnliftIO m, Fractional i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue i)
readRealAttribute = readAttributeSimple' extractReal (convertGenericScalar id)

-- | Read a spectrum attribute irrespective of the concrete real element type. This just uses 'realToFrac' internally.
readRealSpectrumAttribute :: (MonadUnliftIO m, Fractional i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue [i])
readRealSpectrumAttribute = readAttributeSimple' extractReal convertGenericSpectrum'

-- | Read a spectrum image attribute irrespective of the concrete integral element type. This just uses 'realToFrac' internally.
readRealImageAttribute :: (MonadUnliftIO m, Fractional i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue (Image i))
readRealImageAttribute = readAttributeSimple' extractReal convertGenericImage'

extractBool :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CBool)
extractBool (HaskellAttributeDataBoolArray a) = Just a
extractBool _ = Nothing

-- | Read a boolean-type scalar attribute, fail hard if it's not really a bool
readBoolAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Bool)
readBoolAttribute = readAttributeSimple extractBool (convertGenericScalar cboolToBool)

-- | Read a boolean-type spectrum (list) attribute, fail hard if it's not really a bool
readBoolSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Bool])
readBoolSpectrumAttribute = readAttributeSimple extractBool (convertGenericSpectrum cboolToBool)

-- | Read a boolean-type image attribute, fail hard if it's not really a bool
readBoolImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Bool))
readBoolImageAttribute = readAttributeSimple extractBool (convertGenericImage cboolToBool)

-- | Read a string attribute and decode it into a text, fail hard if it's not really a string.
readStringAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Text)
readStringAttribute = readAttributeSimple extract convert
  where
    extract (HaskellAttributeDataStringArray a) = Just a
    extract _ = Nothing
    convert _ (AtLeastTwo read write []) = TangoValue <$> peekCStringText read <*> peekCStringText write
    convert _ _ = error "expected a read and a write value for attribute, got more elements"

-- | Read a string spectrum (array/list) attribute and decode it into a text
readStringSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Text])
readStringSpectrumAttribute = readAttributeSimple extract convert
  where
    extract (HaskellAttributeDataStringArray a) = Just a
    extract _ = Nothing
    convert (HaskellAttributeData {dimX}) (AtLeastTwo first second remainder) = do
      wholeList <- traverse peekCStringText (first : second : remainder)
      let (readValue, writeValue) = splitAt (fromIntegral dimX) wholeList
      pure (TangoValue readValue writeValue)

-- | Read a string image attribute and decode it into a text
readStringImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Text))
readStringImageAttribute = readAttributeSimple extract convert
  where
    extract (HaskellAttributeDataStringArray a) = Just a
    extract _ = Nothing
    convert (HaskellAttributeData {dimX, dimY}) (AtLeastTwo first second remainder) = do
      wholeList <- traverse peekCStringText (first : second : remainder)
      let (readValue, writeValue) = splitAt (fromIntegral (dimX * dimY)) wholeList
      pure
        ( TangoValue
            (Image readValue (fromIntegral dimX) (fromIntegral dimY))
            (Image writeValue (fromIntegral dimX) (fromIntegral dimY))
        )

extractShort :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CShort)
extractShort (HaskellAttributeDataShortArray a) = Just a
extractShort _ = Nothing

-- | Read a short-type scalar attribute, fail hard if it's not really a short
readShortAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Int16)
readShortAttribute = readAttributeSimple extractShort (convertGenericScalar fromIntegral)

-- | Read a short-type spectrum (list) attribute, fail hard if it's not really a short
readShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Int16])
readShortSpectrumAttribute = readAttributeSimple extractShort (convertGenericSpectrum fromIntegral)

-- | Read a short-type image attribute, fail hard if it's not really a short
readShortImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Int16))
readShortImageAttribute = readAttributeSimple extractShort (convertGenericImage fromIntegral)

extractUShort :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CUShort)
extractUShort (HaskellAttributeDataUShortArray a) = Just a
extractUShort _ = Nothing

-- | Read an unsigned short-type scalar attribute, fail hard if it's not really an unsigned short
readUShortAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Word16)
readUShortAttribute = readAttributeSimple extractUShort (convertGenericScalar fromIntegral)

-- | Read an unsigned short-type spectrum (list) attribute, fail hard if it's not really an unsigned short
readUShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Word16])
readUShortSpectrumAttribute = readAttributeSimple extractUShort (convertGenericSpectrum fromIntegral)

-- | Read an unsigned short-type image attribute, fail hard if it's not really an unsigned short
readUShortImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Word16))
readUShortImageAttribute = readAttributeSimple extractUShort (convertGenericImage fromIntegral)

extractLong :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CLong)
extractLong (HaskellAttributeDataLongArray a) = Just a
extractLong _ = Nothing

-- | Read a long-type scalar attribute, fail hard if it's not really a long
readLongAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Int64)
readLongAttribute = readAttributeSimple extractLong (convertGenericScalar fromIntegral)

-- | Read a long-type spectrum (list) attribute, fail hard if it's not really a long
readLongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Int64])
readLongSpectrumAttribute = readAttributeSimple extractLong (convertGenericSpectrum fromIntegral)

-- | Read a long-type image attribute, fail hard if it's not really a long
readLongImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Int64))
readLongImageAttribute = readAttributeSimple extractLong (convertGenericImage fromIntegral)

extractULong :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CULong)
extractULong (HaskellAttributeDataULongArray a) = Just a
extractULong _ = Nothing

-- | Read an unsigned long-type scalar attribute, fail hard if it's not really an unsigned long
readULongAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Word64)
readULongAttribute = readAttributeSimple extractULong (convertGenericScalar fromIntegral)

-- | Read an unsigned long-type spectrum (list) attribute, fail hard if it's not really an unsigned long
readULongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Word64])
readULongSpectrumAttribute = readAttributeSimple extractULong (convertGenericSpectrum fromIntegral)

-- | Read an unsigned long-type image attribute, fail hard if it's not really an unsigned long
readULongImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Word64))
readULongImageAttribute = readAttributeSimple extractULong (convertGenericImage fromIntegral)

extractLong64 :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CLong)
extractLong64 (HaskellAttributeDataLong64Array a) = Just a
extractLong64 _ = Nothing

-- | Read a long64-type scalar attribute, fail hard if it's not really a long64
readLong64Attribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Int64)
readLong64Attribute = readAttributeSimple extractLong64 (convertGenericScalar fromIntegral)

-- | Read a long64-type spectrum (list) attribute, fail hard if it's not really a long64
readLong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Int64])
readLong64SpectrumAttribute = readAttributeSimple extractLong64 (convertGenericSpectrum fromIntegral)

-- | Read a long64-type image attribute, fail hard if it's not really a long64
readLong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Int64))
readLong64ImageAttribute = readAttributeSimple extractLong64 (convertGenericImage fromIntegral)

extractULong64 :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CULong)
extractULong64 (HaskellAttributeDataULong64Array a) = Just a
extractULong64 _ = Nothing

-- | Read an unsigned long64-type scalar attribute, fail hard if it's not really an unsigned long64
readULong64Attribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Word64)
readULong64Attribute = readAttributeSimple extractULong64 (convertGenericScalar fromIntegral)

-- | Read an unsigned long64-type spectrum (list) attribute, fail hard if it's not really an unsigned long64
readULong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Word64])
readULong64SpectrumAttribute = readAttributeSimple extractULong64 (convertGenericSpectrum fromIntegral)

-- | Read an unsigned long64-type image attribute, fail hard if it's not really an unsigned long64
readULong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Word64))
readULong64ImageAttribute = readAttributeSimple extractULong64 (convertGenericImage fromIntegral)

extractFloat :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CFloat)
extractFloat (HaskellAttributeDataFloatArray a) = Just a
extractFloat _ = Nothing

-- | Read a float-type scalar attribute, fail hard if it's not really a float
readFloatAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Double)
readFloatAttribute = readAttributeSimple extractFloat (convertGenericScalar realToFrac)

-- | Read a float-type spectrum (list) attribute, fail hard if it's not really a float
readFloatSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Double])
readFloatSpectrumAttribute = readAttributeSimple extractFloat (convertGenericSpectrum realToFrac)

-- | Read a float-type image attribute, fail hard if it's not really a float
readFloatImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Double))
readFloatImageAttribute = readAttributeSimple extractFloat (convertGenericImage realToFrac)

extractDouble :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CDouble)
extractDouble (HaskellAttributeDataDoubleArray a) = Just a
extractDouble _ = Nothing

-- | Read a double-type scalar attribute, fail hard if it's not really a double
readDoubleAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Double)
readDoubleAttribute = readAttributeSimple extractDouble (convertGenericScalar realToFrac)

-- | Read a double-type spectrum (list) attribute, fail hard if it's not really a double
readDoubleSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Double])
readDoubleSpectrumAttribute = readAttributeSimple extractDouble (convertGenericSpectrum realToFrac)

-- | Read a double-type image attribute, fail hard if it's not really a double
readDoubleImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Double))
readDoubleImageAttribute = readAttributeSimple extractDouble (convertGenericImage realToFrac)

extractState :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray HaskellTangoDevState)
extractState (HaskellAttributeDataStateArray a) = Just a
extractState _ = Nothing

-- | Read a state-type scalar attribute, fail hard if it's not really a state
readStateAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue HaskellTangoDevState)
readStateAttribute = readAttributeSimple extractState (convertGenericScalar id)

-- | Read a state-type spectrum (list) attribute, fail hard if it's not really a state type
readStateSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [HaskellTangoDevState])
readStateSpectrumAttribute = readAttributeSimple extractState (convertGenericSpectrum id)

-- | Read a state-type image attribute, fail hard if it's not really a state
readStateImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image HaskellTangoDevState))
readStateImageAttribute = readAttributeSimple extractState (convertGenericImage id)

extractEnum :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CShort)
extractEnum (HaskellAttributeDataShortArray a) = Just a
extractEnum _ = Nothing

-- | Read an enum-type scalar attribute, fail hard if it's not really an enum (internally, enums are shorts)
readEnumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (TangoValue t)
readEnumAttribute = readAttributeSimple extractEnum (convertGenericScalar (toEnum . fromIntegral))

-- | Read an enum-type spectrum attribute, fail hard if it's not really an enum (internally, enums are shorts)
readEnumSpectrumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (TangoValue [t])
readEnumSpectrumAttribute = readAttributeSimple extractEnum (convertGenericSpectrum (toEnum . fromIntegral))

-- | Read an enum-type image attribute, fail hard if it's not really an enum (internally, enums are shorts)
readEnumImageAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (TangoValue (Image t))
readEnumImageAttribute = readAttributeSimple extractEnum (convertGenericImage (toEnum . fromIntegral))

-- | Newtype wrapper around a command name
newtype CommandName = CommandName Text

instance Show CommandName where
  show (CommandName n) = show (unpack n)

-- | Input and output data for a command
data CommandData
  = CommandVoid
  | CommandBool !Bool
  | CommandShort !Int16
  | CommandUShort !Word16
  | CommandInt32 !Int32
  | CommandInt64 !Int64
  | CommandWord64 !Word64
  | CommandFloat !Float
  | CommandDouble !Double
  | CommandString !Text
  | CommandState !HaskellTangoDevState
  | CommandEnum !Int16
  | CommandListBool ![Bool]
  | CommandListShort ![Int16]
  | CommandListUShort ![Word16]
  | CommandListInt64 ![Int64]
  | CommandListWord64 ![Word64]
  | CommandListLong64 ![Int64]
  | CommandListULong64 ![Word64]
  | CommandListFloat ![Float]
  | CommandListDouble ![Double]
  | CommandListString ![Text]
  | CommandListState ![HaskellTangoDevState]
  deriving (Show)

-- | Execute command with no input and no output
commandInVoidOutVoid :: (MonadUnliftIO m) => DeviceProxy -> CommandName -> m ()
commandInVoidOutVoid (DeviceProxy proxyPtr) (CommandName commandName) =
  liftIO $
    withCStringText
      commandName
      \commandNamePtr ->
        with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \commandDataInPtr -> with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \commandDataOutPtr ->
          checkResult $ tango_command_inout proxyPtr commandNamePtr commandDataInPtr commandDataOutPtr

withVarArray :: (MonadUnliftIO m, Storable a) => [a] -> (HaskellTangoVarArray a -> m b) -> m b
withVarArray b f = withArray b (f . HaskellTangoVarArray (fromIntegral (length b)))

newCStringText :: (MonadUnliftIO m) => Text -> m CString
newCStringText = newCString . unpack

withRawCommandData :: (MonadUnliftIO m) => CommandData -> (Ptr HaskellCommandData -> m a) -> m a
withRawCommandData CommandVoid f = with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) f
withRawCommandData (CommandBool b) f = with (HaskellCommandData HaskellDevBoolean (HaskellCommandBool (boolToCBool b))) f
withRawCommandData (CommandShort b) f = with (HaskellCommandData HaskellDevShort (HaskellCommandShort (fromIntegral b))) f
withRawCommandData (CommandUShort b) f = with (HaskellCommandData HaskellDevUShort (HaskellCommandUShort (fromIntegral b))) f
withRawCommandData (CommandInt32 b) f = with (HaskellCommandData HaskellDevInt (HaskellCommandInt32 (fromIntegral b))) f
withRawCommandData (CommandInt64 b) f = with (HaskellCommandData HaskellDevShort (HaskellCommandULong64 (fromIntegral b))) f
withRawCommandData (CommandWord64 b) f = with (HaskellCommandData HaskellDevUShort (HaskellCommandLong64 (fromIntegral b))) f
withRawCommandData (CommandFloat b) f = with (HaskellCommandData HaskellDevFloat (HaskellCommandFloat (realToFrac b))) f
withRawCommandData (CommandDouble b) f = with (HaskellCommandData HaskellDevDouble (HaskellCommandDouble (realToFrac b))) f
withRawCommandData (CommandString t) f =
  bracket
    (newCString (unpack t))
    free
    (\s -> with (HaskellCommandData HaskellDevString (HaskellCommandCString s)) f)
withRawCommandData (CommandState b) f = with (HaskellCommandData HaskellDevState (HaskellCommandDevState b)) f
withRawCommandData (CommandEnum b) f = with (HaskellCommandData HaskellDevEnum (HaskellCommandDevEnum (fromIntegral (fromEnum b)))) f
withRawCommandData (CommandListBool b) f =
  withVarArray
    (boolToCBool <$> b)
    \varList ->
      with (HaskellCommandData HaskellDevVarBooleanArray (HaskellCommandVarBool varList)) f
withRawCommandData (CommandListShort b) f =
  withVarArray
    (fromIntegral <$> b)
    \varList ->
      with (HaskellCommandData HaskellDevVarShortArray (HaskellCommandVarShort varList)) f
withRawCommandData (CommandListUShort b) f =
  withVarArray
    (fromIntegral <$> b)
    \varList ->
      with (HaskellCommandData HaskellDevVarUShortArray (HaskellCommandVarUShort varList)) f
withRawCommandData (CommandListInt64 b) f =
  withVarArray
    (fromIntegral <$> b)
    \varList ->
      with (HaskellCommandData HaskellDevVarLongArray (HaskellCommandVarLong varList)) f
withRawCommandData (CommandListLong64 b) f =
  withVarArray
    (fromIntegral <$> b)
    \varList ->
      with (HaskellCommandData HaskellDevVarLong64Array (HaskellCommandVarLong64 varList)) f
withRawCommandData (CommandListWord64 b) f =
  withVarArray
    (fromIntegral <$> b)
    \varList ->
      with (HaskellCommandData HaskellDevVarULongArray (HaskellCommandVarULong varList)) f
withRawCommandData (CommandListULong64 b) f =
  withVarArray
    (fromIntegral <$> b)
    \varList ->
      with (HaskellCommandData HaskellDevVarULong64Array (HaskellCommandVarULong64 varList)) f
withRawCommandData (CommandListFloat b) f =
  withVarArray
    (realToFrac <$> b)
    \varList ->
      with (HaskellCommandData HaskellDevVarFloatArray (HaskellCommandVarFloat varList)) f
withRawCommandData (CommandListDouble b) f =
  withVarArray
    (realToFrac <$> b)
    \varList ->
      with (HaskellCommandData HaskellDevVarDoubleArray (HaskellCommandVarDouble varList)) f
withRawCommandData (CommandListString texts) f =
  UnliftIO.bracket
    (traverse newCStringText texts)
    (traverse free)
    ( \textPtrList -> withVarArray
        textPtrList
        \varList ->
          with (HaskellCommandData HaskellDevVarStringArray (HaskellCommandVarCString varList)) f
    )
withRawCommandData (CommandListState b) f =
  withVarArray
    b
    \varList ->
      with (HaskellCommandData HaskellDevVarStateArray (HaskellCommandVarDevState varList)) f

tangoVarArrayToList :: (MonadUnliftIO m, Storable a) => HaskellTangoVarArray a -> m [a]
tangoVarArrayToList (HaskellTangoVarArray {varArrayLength, varArrayValues}) =
  peekArray (fromIntegral varArrayLength) varArrayValues

fromRawCommandData :: (MonadUnliftIO m) => HaskellCommandData -> m (Maybe CommandData)
fromRawCommandData (HaskellCommandData {tangoCommandData}) =
  case tangoCommandData of
    HaskellCommandVoid -> pure $ Just CommandVoid
    HaskellCommandBool cbool -> pure $ Just $ CommandBool $ cboolToBool cbool
    HaskellCommandShort v -> pure $ Just $ CommandShort $ fromIntegral v
    HaskellCommandUShort v -> pure $ Just $ CommandUShort $ fromIntegral v
    HaskellCommandFloat v -> pure $ Just $ CommandFloat $ realToFrac v
    HaskellCommandDouble v -> pure $ Just $ CommandDouble $ realToFrac v
    HaskellCommandCString v -> Just . CommandString . pack <$> peekCString v
    HaskellCommandInt32 v -> pure $ Just $ CommandInt32 $ fromIntegral v
    HaskellCommandLong64 v -> pure $ Just $ CommandInt64 $ fromIntegral v
    HaskellCommandDevState v -> pure $ Just $ CommandState v
    HaskellCommandULong64 v -> pure $ Just $ CommandWord64 $ fromIntegral v
    HaskellCommandDevEnum v -> pure $ Just $ CommandEnum $ toEnum $ fromIntegral v
    HaskellCommandVarBool a -> Just . CommandListBool . (cboolToBool <$>) <$> tangoVarArrayToList a
    HaskellCommandVarShort a -> Just . CommandListShort . (fromIntegral <$>) <$> tangoVarArrayToList a
    HaskellCommandVarUShort a -> Just . CommandListUShort . (fromIntegral <$>) <$> tangoVarArrayToList a
    HaskellCommandVarLong a -> Just . CommandListInt64 . (fromIntegral <$>) <$> tangoVarArrayToList a
    HaskellCommandVarULong a -> Just . CommandListWord64 . (fromIntegral <$>) <$> tangoVarArrayToList a
    HaskellCommandVarLong64 a -> Just . CommandListLong64 . (fromIntegral <$>) <$> tangoVarArrayToList a
    HaskellCommandVarULong64 a -> Just . CommandListULong64 . (fromIntegral <$>) <$> tangoVarArrayToList a
    HaskellCommandVarFloat a -> Just . CommandListFloat . (realToFrac <$>) <$> tangoVarArrayToList a
    HaskellCommandVarDouble a -> Just . CommandListDouble . (realToFrac <$>) <$> tangoVarArrayToList a
    HaskellCommandVarCString strings -> do
      stringsAsList <- tangoVarArrayToList strings
      texts <- traverse peekCStringText stringsAsList
      pure (Just (CommandListString texts))
    HaskellCommandVarDevState a -> Just . CommandListState <$> tangoVarArrayToList a
    _ -> pure Nothing

-- | Execute command with generic input and generic output. If you have an @Enum@ on hand, use 'commandInEnumOutGeneric', 'commandInGenericOutEnum' and 'commandInEnumOutEnum'
commandInOutGeneric :: (MonadUnliftIO m) => DeviceProxy -> CommandName -> CommandData -> m CommandData
commandInOutGeneric (DeviceProxy proxyPtr) (CommandName commandName) in' =
  liftIO $
    withCStringText commandName $
      \commandNamePtr ->
        withRawCommandData in' \commandDataInPtr ->
          with (RawCommon.HaskellCommandData RawCommon.HaskellDevVoid RawCommon.HaskellCommandVoid) $ \commandDataOutPtr -> do
            checkResult $ tango_command_inout proxyPtr commandNamePtr commandDataInPtr commandDataOutPtr
            outValue <- peek commandDataOutPtr
            result <- fromRawCommandData outValue
            case result of
              Nothing -> error "couldn't convert the command out value"
              Just result' -> pure result'

-- | Execute command with /enum/ input and generic output (special case to handle arbitrary enums)
commandInEnumOutGeneric :: (MonadUnliftIO m, Enum t) => DeviceProxy -> CommandName -> t -> m CommandData
commandInEnumOutGeneric proxy commandName in' = commandInOutGeneric proxy commandName (CommandShort $ fromIntegral $ fromEnum in')

-- | Execute command with generic input and /enum/ output (special case to handle arbitrary enums)
commandInGenericOutEnum :: (MonadUnliftIO m, Enum t) => DeviceProxy -> CommandName -> CommandData -> m t
commandInGenericOutEnum proxy commandName in' = do
  result <- commandInOutGeneric proxy commandName in'
  case result of
    CommandShort s -> pure (toEnum (fromIntegral s))
    _ -> error ("command " <> show commandName <> " was supposed to return a short (for enums), but returned " <> show result)

-- | Execute command with /enum input and /enum/ output (special case to handle arbitrary enums)
commandInEnumOutEnum :: (MonadUnliftIO m, Enum t, Enum u) => DeviceProxy -> CommandName -> u -> m t
commandInEnumOutEnum proxy commandName in' = do
  result <- commandInOutGeneric proxy commandName (CommandShort $ fromIntegral $ fromEnum in')
  case result of
    CommandShort s -> pure (toEnum (fromIntegral s))
    _ -> error ("command " <> show commandName <> " was supposed to return a short (for enums), but returned " <> show result)

-- throwTangoException :: (MonadIO m) => Text -> m ()
-- throwTangoException desc = do
--   str <- newCString (unpack desc)
--   liftIO $ tango_throw_exception str

-- | Information for a single attribute (for spectrum and images as well, see the dimensions)
data AttributeInfo = AttributeInfo
  { attributeInfoWritable :: !HaskellAttrWriteType,
    attributeInfoDataFormat :: !HaskellDataFormat,
    attributeInfoDataType :: !HaskellTangoDataType,
    attributeInfoMaxDimX :: !Int,
    attributeInfoMaxDimY :: !Int,
    attributeInfoDescription :: !Text,
    attributeInfoLabel :: !Text,
    attributeInfoUnit :: !Text,
    attributeInfoStandardUnit :: !Text,
    attributeInfoDisplayUnit :: !Text,
    attributeInfoFormat :: !Text,
    attributeInfoMinValue :: !Text,
    attributeInfoMaxValue :: !Text,
    attributeInfoMinAlarm :: !Text,
    attributeInfoMaxAlarm :: !Text,
    attributeInfoWritableAttrName :: !Text,
    attributeInfoDispLevel :: !HaskellDispLevel,
    attributeInfoEnumLabels :: [Text],
    -- | Root attribute name (in case of forwarded attribute)
    attributeInfoRootAttrName :: Text,
    attributeInfoMemorized :: !TangoAttrMemorizedType
  }
  deriving (Show)

convertAttributeInfo :: forall m. (MonadUnliftIO m) => RawCommon.HaskellAttributeInfo -> m AttributeInfo
convertAttributeInfo ai = do
  description <- peekCStringText (RawCommon.attributeInfoDescription ai)
  label <- peekCStringText (RawCommon.attributeInfoLabel ai)
  unit <- peekCStringText (RawCommon.attributeInfoUnit ai)
  standardUnit <- peekCStringText (RawCommon.attributeInfoStandardUnit ai)
  displayUnit <- peekCStringText (RawCommon.attributeInfoDisplayUnit ai)
  format <- peekCStringText (RawCommon.attributeInfoFormat ai)
  minValue <- peekCStringText (RawCommon.attributeInfoMinValue ai)
  maxValue <- peekCStringText (RawCommon.attributeInfoMaxValue ai)
  minAlarm <- peekCStringText (RawCommon.attributeInfoMinAlarm ai)
  maxAlarm <- peekCStringText (RawCommon.attributeInfoMaxAlarm ai)
  writableAttrName <- peekCStringText (RawCommon.attributeInfoWritableAttrName ai)
  enumLabelsList <-
    peekCStringArrayText (RawCommon.attributeInfoEnumLabelsCount ai) (RawCommon.attributeInfoEnumLabels ai)
  rootAttrName <- peekCStringText (RawCommon.attributeInfoRootAttrName ai)
  pure $
    AttributeInfo
      (RawCommon.attributeInfoWritable ai)
      (RawCommon.attributeInfoDataFormat ai)
      (RawCommon.attributeInfoDataType ai)
      (fromIntegral (RawCommon.attributeInfoMaxDimX ai))
      (fromIntegral (RawCommon.attributeInfoMaxDimY ai))
      description
      label
      unit
      standardUnit
      displayUnit
      format
      minValue
      maxValue
      minAlarm
      maxAlarm
      writableAttrName
      (RawCommon.attributeInfoDispLevel ai)
      enumLabelsList
      rootAttrName
      (RawCommon.attributeInfoMemorized ai)

-- | Get information on a single attribute (this uses 'getConfigsForAttributes' internally)
getConfigForAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m AttributeInfo
getConfigForAttribute proxy attributeName =
  head <$> liftIO (getConfigsForAttributes proxy [attributeName])

-- | Get information for a set of attributes (see 'getConfigForAttribute' for a single attribute)
getConfigsForAttributes :: forall m. (MonadUnliftIO m) => DeviceProxy -> [AttributeName] -> m [AttributeInfo]
getConfigsForAttributes (DeviceProxy deviceProxyPtr) attributeNames = do
  let attributeNameToCString :: AttributeName -> m CString
      attributeNameToCString (AttributeName t) = newCString (unpack t)
  bracket (traverse attributeNameToCString attributeNames) (traverse free) \cstringList ->
    withArray cstringList \cstringPtr ->
      with (HaskellTangoVarArray (fromIntegral (length attributeNames)) cstringPtr) \varArrayPtr ->
        with (HaskellAttributeInfoList 0 nullPtr) \outputPtr ->
          bracket
            (checkResult (liftIO (tango_get_attribute_config deviceProxyPtr varArrayPtr outputPtr)))
            (\_ -> liftIO (tango_free_AttributeInfoList outputPtr))
            \_ -> do
              outputPeeked <- liftIO (peek outputPtr)
              elements <- peekArray (fromIntegral (attributeInfoListLength outputPeeked)) (attributeInfoListSequence outputPeeked)
              traverse convertAttributeInfo elements

-- | Newtype wrapper around a property name
newtype PropertyName = PropertyName Text

instance Show PropertyName where
  show (PropertyName x) = show (unpack x)

-- | All data stored for a property in Tango
data Property = Property
  { propertyName :: !Text,
    propertyIsEmpty :: !Bool,
    propertyWrongDataType :: !Bool,
    propertyData :: ![Text]
  }
  deriving (Show)

convertPropertyData :: (MonadUnliftIO m) => HaskellTangoPropertyData -> m [Text]
convertPropertyData (HaskellPropStringArray v) = tangoVarArrayToList v >>= traverse peekCStringText
convertPropertyData v = error $ "couldn't convert property data to Haskell: " <> show v

convertDbDatum :: (MonadUnliftIO m) => HaskellDbDatum -> m Property
convertDbDatum dbDatum = do
  propData <- convertPropertyData (dbDatumPropData dbDatum)
  nameConverted <- peekCStringText (dbDatumPropertyName dbDatum)
  pure $
    Property
      nameConverted
      (dbDatumIsEmpty dbDatum)
      (dbDatumWrongDataType dbDatum)
      propData

nameToDbDatum :: (MonadUnliftIO m) => PropertyName -> m HaskellDbDatum
nameToDbDatum (PropertyName name) = do
  nameCString <- newCString (unpack name)
  pure (HaskellDbDatum nameCString False False HaskellDevVarStringArray (HaskellPropStringArray (HaskellTangoVarArray 0 nullPtr)))

freeDbDatum :: (MonadUnliftIO m) => HaskellDbDatum -> m ()
freeDbDatum dbDatum = do
  dbDatumPtr <- new dbDatum
  -- free (dbDatumPropertyName dbDatum)
  liftIO (tango_free_DbDatum dbDatumPtr)

-- | Get a list of information for the given property names
getDeviceProperties :: forall m. (MonadUnliftIO m) => DeviceProxy -> [PropertyName] -> m [Property]
getDeviceProperties (DeviceProxy proxyPtr) names =
  let initialize :: m [HaskellDbDatum]
      initialize = traverse nameToDbDatum names
      destroy :: [HaskellDbDatum] -> m ()
      destroy = void . traverse freeDbDatum
   in UnliftIO.bracket
        initialize
        destroy
        \dbDatumPtrListIn ->
          withArray dbDatumPtrListIn \dbDatumPtrIn ->
            liftIO $ with
              (HaskellDbData (fromIntegral (length names)) dbDatumPtrIn)
              \dbDataPtr -> do
                checkResult (liftIO (tango_get_device_property proxyPtr dbDataPtr))
                dbData <- liftIO (peek dbDataPtr)
                dbDatumPtrListOut <- peekArray (fromIntegral (dbDataLength dbData)) (dbDataSequence dbData)
                traverse convertDbDatum dbDatumPtrListOut

textListToVarArray :: (MonadUnliftIO m) => [Text] -> m (HaskellTangoVarArray CString)
textListToVarArray texts = do
  cstringList :: [CString] <- traverse newCStringText texts
  cStringPtr :: Ptr CString <- newArray cstringList
  pure (HaskellTangoVarArray (fromIntegral (length texts)) cStringPtr)

nameAndValueToDbDatum :: (MonadUnliftIO m) => (PropertyName, [Text]) -> m HaskellDbDatum
nameAndValueToDbDatum (PropertyName name, texts) = do
  varArray <- textListToVarArray texts
  nameCString <- newCString (unpack name)
  pure (HaskellDbDatum nameCString False False HaskellDevVarStringArray (HaskellPropStringArray varArray))

-- | Change property values for the device (here with a crude pair)
putDeviceProperties :: forall m. (MonadUnliftIO m) => DeviceProxy -> [(PropertyName, [Text])] -> m ()
putDeviceProperties (DeviceProxy proxyPtr) namesAndValues =
  bracket
    (traverse nameAndValueToDbDatum namesAndValues)
    (void . traverse freeDbDatum)
    \dbDatumPtrListIn ->
      withArray dbDatumPtrListIn \dbDatumPtrIn ->
        liftIO $
          with
            (HaskellDbData (fromIntegral (length namesAndValues)) dbDatumPtrIn)
            (checkResult . liftIO . tango_put_device_property proxyPtr)

-- | Delete the given device properties
deleteDeviceProperties :: forall m. (MonadUnliftIO m) => DeviceProxy -> [PropertyName] -> m ()
deleteDeviceProperties (DeviceProxy proxyPtr) names =
  let initialize :: m [HaskellDbDatum]
      initialize = traverse nameToDbDatum names
      destroy :: [HaskellDbDatum] -> m ()
      destroy = void . traverse freeDbDatum
   in bracket
        initialize
        destroy
        \dbDatumPtrListIn ->
          withArray dbDatumPtrListIn \dbDatumPtrIn ->
            liftIO $
              with
                (HaskellDbData (fromIntegral (length names)) dbDatumPtrIn)
                (checkResult . liftIO . tango_delete_device_property proxyPtr)

-- | Lock the device (see 'withLocked' for an exception-safe version of this)
lockDevice :: (MonadUnliftIO m) => DeviceProxy -> m ()
lockDevice (DeviceProxy proxyPtr) = (checkResult . liftIO . tango_lock) proxyPtr

-- | Unlock the device (see 'withLocked' for an exception-safe version of this)
unlockDevice :: (MonadUnliftIO m) => DeviceProxy -> m ()
unlockDevice (DeviceProxy proxyPtr) = (checkResult . liftIO . tango_unlock) proxyPtr

-- | Execute the given action with a locked device (see 'lockDevice' and 'unlockDevice')
withLocked :: (MonadUnliftIO m) => DeviceProxy -> m () -> m ()
withLocked device f = bracket (lockDevice device) (\_ -> unlockDevice device) (const f)

-- | Newtype wrapper around milliseconds to make the raw numbers a bit more readable
newtype Milliseconds = Milliseconds Int

instance Show Milliseconds where
  show (Milliseconds ms) = show ms <> "ms"

-- | Set timeout for this device (relates to most operations: reading an attribute, executing a command)
setTimeout :: (MonadUnliftIO m) => DeviceProxy -> Milliseconds -> m ()
setTimeout (DeviceProxy proxy) (Milliseconds ms) = liftIO $ checkResult $ tango_set_timeout_millis proxy (fromIntegral ms)

-- | Get current timeout for this device
getTimeout :: (MonadUnliftIO m) => DeviceProxy -> m Milliseconds
getTimeout (DeviceProxy proxy) = liftIO $ with 0 \intPtr -> do
  checkResult (tango_get_timeout_millis proxy intPtr)
  intValue <- peek intPtr
  pure (Milliseconds (fromIntegral intValue))

-- | Enable polling for a command (see 'stopPollCommand' to stop it)
pollCommand :: (MonadUnliftIO m) => DeviceProxy -> CommandName -> Milliseconds -> m ()
pollCommand (DeviceProxy proxy) (CommandName commandName) (Milliseconds ms) = liftIO $ withCStringText commandName \commandNameC -> checkResult (tango_poll_command proxy commandNameC (fromIntegral ms))

-- | Disable polling for a command (see 'pollCommand')
stopPollCommand :: (MonadUnliftIO m) => DeviceProxy -> CommandName -> m ()
stopPollCommand (DeviceProxy proxy) (CommandName commandName) = liftIO $ withCStringText commandName (checkResult . tango_stop_poll_command proxy)

-- | Enable polling for an attribute (see 'stopPollAttribute' to stop it)
pollAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Milliseconds -> m ()
pollAttribute (DeviceProxy proxy) (AttributeName attributeName) (Milliseconds ms) = liftIO $ withCStringText attributeName \attributeNameC -> checkResult (tango_poll_attribute proxy attributeNameC (fromIntegral ms))

-- | Disable polling for an attribute (see 'pollAttribute')
stopPollAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m ()
stopPollAttribute (DeviceProxy proxy) (AttributeName attributeName) = liftIO $ withCStringText attributeName (checkResult . tango_stop_poll_attribute proxy)

-- | Where to display this command (in Jive, for example)
data DisplayLevel = Operator | Expert deriving (Show, Enum, Bounded, Eq)

-- | All information Tango has on a command
data CommandInfo = CommandInfo
  { commandInfoName :: !Text,
    commandInfoTag :: !Int,
    commandInfoInType :: !HaskellTangoDataType,
    commandInfoOutType :: !HaskellTangoDataType,
    commandInfoInTypeDesc :: !Text,
    commandInfoOutTypeDesc :: !Text,
    commandInfoDisplayLevel :: !DisplayLevel
  }
  deriving (Show)

convertCommandInfo :: HaskellCommandInfo -> IO CommandInfo
convertCommandInfo (HaskellCommandInfo {cmdName, cmdTag, cmdInType, cmdOutType, cmdInTypeDesc, cmdOutTypeDesc, cmdDisplayLevel}) =
  CommandInfo
    <$> peekCStringText cmdName
    <*> pure (fromIntegral cmdTag)
    <*> pure (toEnum (fromIntegral cmdInType))
    <*> pure (toEnum (fromIntegral cmdOutType))
    <*> peekCStringText cmdInTypeDesc
    <*> peekCStringText cmdOutTypeDesc
    <*> pure (toEnum (fromIntegral cmdDisplayLevel))

-- | Get a list of all commands for the device (see 'commandQuery' if you know the command name)
commandListQuery :: (MonadUnliftIO m) => DeviceProxy -> m [CommandInfo]
commandListQuery (DeviceProxy proxy) = liftIO $ with (HaskellCommandInfoList 0 nullPtr) \infoListPtr -> do
  checkResult (tango_command_list_query proxy infoListPtr)
  infoList <- peek infoListPtr
  converted <- peekArray (fromIntegral (commandInfoLength infoList)) (commandInfoSequence infoList)
  result <- traverse convertCommandInfo converted
  tango_free_CommandInfoList infoListPtr
  pure result

-- | Get info for a single command of the device (see 'commandListQuery' for all commands)
commandQuery :: (MonadUnliftIO m) => DeviceProxy -> CommandName -> m CommandInfo
commandQuery (DeviceProxy proxy) (CommandName commandName) = liftIO $ withCStringText commandName \commandNamePtr -> alloca \commandInfoPtr -> do
  checkResult (tango_command_query proxy commandNamePtr commandInfoPtr)
  commandInfoC <- peek commandInfoPtr
  result <- convertCommandInfo commandInfoC
  tango_free_CommandInfo commandInfoPtr
  pure result

-- | Get a list of all attributes inside the device
getAttributeNames :: (MonadUnliftIO m) => DeviceProxy -> m [AttributeName]
getAttributeNames (DeviceProxy proxy) = liftIO $ with (HaskellTangoVarArray 0 nullPtr) \nameListPtr -> do
  checkResult (tango_get_attribute_list proxy nameListPtr)
  nameList <- peek nameListPtr
  names <- peekArray (fromIntegral (varArrayLength nameList)) (varArrayValues nameList)
  result <- traverse ((AttributeName <$>) . peekCStringText) names
  tango_free_VarStringArray nameListPtr
  pure result

-- | Create a proxy for the Tango DB (not the same as a device proxy), see 'deleteDatabaseProxy' and 'withDatabaseProxy'
createDatabaseProxy :: (MonadUnliftIO m) => m DatabaseProxy
createDatabaseProxy = liftIO $ alloca \databaseProxyPtrPtr -> do
  checkResult (tango_create_database_proxy databaseProxyPtrPtr)
  peek databaseProxyPtrPtr

-- | Delete proxy for the Tango DB, see 'createDatabaseProxy' and 'withDatabaseProxy'
deleteDatabaseProxy :: (MonadUnliftIO m) => DatabaseProxy -> m ()
deleteDatabaseProxy proxy = liftIO (checkResult (tango_delete_database_proxy proxy))

-- | Execute an action safely, on a database proxy, see 'createDatabaseProxy' and 'deleteDatabaseProxy'
withDatabaseProxy :: (MonadUnliftIO m) => (DatabaseProxy -> m a) -> m a
withDatabaseProxy = bracket createDatabaseProxy deleteDatabaseProxy

-- | Search the database for devices with a certain name filter. Can include globs, such as @sys/*@ to search for all devices starting with @sys@.
databaseSearchByDeviceName :: (MonadUnliftIO m) => DatabaseProxy -> Text -> m [Text]
databaseSearchByDeviceName proxy nameFilter = liftIO $ withCStringText nameFilter \deviceNamePtr -> alloca \dbDatumPtr -> do
  checkResult (tango_get_device_exported proxy deviceNamePtr dbDatumPtr)
  dbDatum <- peek dbDatumPtr
  result <- convertDbDatum dbDatum
  tango_free_DbDatum dbDatumPtr
  pure (propertyData result)

-- | Search the database for devices with a certain class
databaseSearchByClass :: (MonadUnliftIO m) => DatabaseProxy -> Text -> m [Text]
databaseSearchByClass proxy classFilter = liftIO $ withCStringText classFilter \classNamePtr -> alloca \dbDatumPtr -> do
  checkResult (tango_get_device_exported_for_class proxy classNamePtr dbDatumPtr)
  dbDatum <- peek dbDatumPtr
  result <- convertDbDatum dbDatum
  tango_free_DbDatum dbDatumPtr
  pure (propertyData result)

-- | Search the database for objects with a certain name (don't know what this is)
databaseSearchObjectsByName :: (MonadUnliftIO m) => DatabaseProxy -> Text -> m [Text]
databaseSearchObjectsByName proxy nameFilter = liftIO $ withCStringText nameFilter \nameFilterPtr -> alloca \dbDatumPtr -> do
  checkResult (tango_get_object_list proxy nameFilterPtr dbDatumPtr)
  dbDatum <- peek dbDatumPtr
  result <- convertDbDatum dbDatum
  tango_free_DbDatum dbDatumPtr
  pure (propertyData result)

-- | I don't know what this is for
databaseSearchObjectPropertiesByName :: (MonadUnliftIO m) => DatabaseProxy -> Text -> Text -> m [Text]
databaseSearchObjectPropertiesByName proxy objectName nameFilter = liftIO $ withCStringText objectName \objectNamePtr -> withCStringText nameFilter \nameFilterPtr -> alloca \dbDatumPtr -> do
  checkResult (tango_get_object_property_list proxy objectNamePtr nameFilterPtr dbDatumPtr)
  dbDatum <- peek dbDatumPtr
  result <- convertDbDatum dbDatum
  tango_free_DbDatum dbDatumPtr
  pure (propertyData result)

-- | Structure holding information on how to unsubscribe from an event again. Feed to 'unsubscribeEvent'
data SubscribedEvent = SubscribedEvent (Ptr ()) CInt

-- | Callback for an event
type EventCallback m =
  AttributeName ->
  -- | @True@ if an error occurred
  Bool ->
  m ()

-- | Subscribe to an event. See 'unsubscribeEvent'
subscribeEvent ::
  forall m.
  (MonadUnliftIO m) =>
  DeviceProxy ->
  AttributeName ->
  EventType ->
  -- | The stateless flag = false indicates that the event subscription will only succeed when the given attribute is known and available in the Tango system. Setting stateless = true will make the subscription succeed, even if an attribute of this name was never known. The real event subscription will happen when the given attribute will be available in the Tango system.
  Bool ->
  EventCallback m ->
  m SubscribedEvent
subscribeEvent (DeviceProxy proxyPtr) attributeName@(AttributeName attributeNameText) eventType stateless eventCallback = withRunInIO \run -> do
  let realCallback :: Ptr () -> CString -> Bool -> IO ()
      realCallback _ _ bool = run (eventCallback attributeName bool)
  convertedCallback <- createEventCallbackWrapper realCallback
  callbackInTango <- tango_create_event_callback convertedCallback
  withCStringText attributeNameText \attributeNameC -> do
    eventId <- tango_subscribe_event proxyPtr attributeNameC (fromIntegral (fromEnum eventType)) callbackInTango (if stateless then 1 else 0)
    pure (SubscribedEvent callbackInTango eventId)

-- | Unsubscribe from the event, see 'subscribeEvent'
unsubscribeEvent :: (MonadUnliftIO m) => DeviceProxy -> SubscribedEvent -> m ()
unsubscribeEvent (DeviceProxy proxyPtr) (SubscribedEvent callbackPtr eventId) = do
  liftIO (tango_unsubscribe_event proxyPtr eventId)
  liftIO (tango_free_event_callback callbackPtr)

-- | Execute an action while being subscribed to the event
withSubscribedEvent ::
  (MonadUnliftIO m) =>
  DeviceProxy ->
  AttributeName ->
  EventType ->
  -- | The stateless flag = false indicates that the event subscription will only succeed when the given attribute is known and available in the Tango system. Setting stateless = true will make the subscription succeed, even if an attribute of this name was never known. The real event subscription will happen when the given attribute will be available in the Tango system.
  Bool ->
  EventCallback m ->
  -- | Action to perform while we have the subscription
  m () ->
  m ()
withSubscribedEvent proxy attributeName eventType stateless eventCallback f =
  bracket (subscribeEvent proxy attributeName eventType stateless eventCallback) (unsubscribeEvent proxy) (const f)
