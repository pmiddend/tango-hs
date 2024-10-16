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
-- == Errors
--
-- Errors are thrown as exceptions of type 'TangoException'. User errors (such as reading a string attribute with a "read int" function) are thrown via 'error' instead.
--
-- == Properties
--
-- The property retrieval API for Tango is elaborate, supporting different data types. We condensed this down to
-- retrieving lists of strings. Conversion needs to happen on the Haskell side for now.
module Tango.Client
  ( -- * Basics and initialization

    --

    -- | To ensure proper cleanup, you should prefer the 'withDeviceProxy' function to initialize a proxy to a device, and then do something with it.
    DeviceProxy,
    TangoUrl,
    parseTangoUrl,
    withDeviceProxy,
    newDeviceProxy,
    deleteDeviceProxy,
    TangoException (TangoException),
    DevFailed (DevFailed),
    devFailedDesc,
    devFailedReason,
    devFailedOrigin,
    devFailedSeverity,

    -- * Attributes
    AttributeName (AttributeName),
    AttributeInfo (..),
    getConfigsForAttributes,
    getConfigForAttribute,
    TangoValue (TangoValue),
    Image (Image, imageContent, imageDimX, imageDimY),
    readIntegralAttribute,
    writeIntegralAttribute,
    readIntegralSpectrumAttribute,
    writeIntegralSpectrumAttribute,
    readIntegralImageAttribute,
    writeIntegralImageAttribute,
    readRealAttribute,
    writeRealAttribute,
    readRealSpectrumAttribute,
    writeRealSpectrumAttribute,
    readRealImageAttribute,
    writeRealImageAttribute,
    readBoolAttribute,
    readBoolSpectrumAttribute,
    readBoolImageAttribute,
    readShortAttribute,
    readShortSpectrumAttribute,
    readShortImageAttribute,
    readUShortAttribute,
    readUShortSpectrumAttribute,
    readUShortImageAttribute,
    readStringAttribute,
    readStringSpectrumAttribute,
    readStringImageAttribute,
    readLongAttribute,
    readLongSpectrumAttribute,
    readLongImageAttribute,
    readULongAttribute,
    readULongSpectrumAttribute,
    readULongImageAttribute,
    readLong64Attribute,
    readLong64SpectrumAttribute,
    readLong64ImageAttribute,
    readULong64Attribute,
    readULong64SpectrumAttribute,
    readULong64ImageAttribute,
    readFloatAttribute,
    readFloatSpectrumAttribute,
    readFloatImageAttribute,
    readDoubleAttribute,
    readDoubleSpectrumAttribute,
    readDoubleImageAttribute,
    readStateAttribute,
    readStateSpectrumAttribute,
    readStateImageAttribute,
    readEnumAttribute,
    readEnumSpectrumAttribute,
    readEnumImageAttribute,
    writeBoolAttribute,
    writeBoolSpectrumAttribute,
    writeBoolImageAttribute,
    writeShortAttribute,
    writeShortSpectrumAttribute,
    writeShortImageAttribute,
    writeUShortAttribute,
    writeUShortSpectrumAttribute,
    writeUShortImageAttribute,
    writeStringAttribute,
    writeStringSpectrumAttribute,
    writeStringImageAttribute,
    writeLongAttribute,
    writeLongSpectrumAttribute,
    writeLongImageAttribute,
    writeULongAttribute,
    writeULongSpectrumAttribute,
    writeULongImageAttribute,
    writeLong64Attribute,
    writeLong64SpectrumAttribute,
    writeLong64ImageAttribute,
    writeULong64Attribute,
    writeULong64SpectrumAttribute,
    writeULong64ImageAttribute,
    writeFloatAttribute,
    writeFloatSpectrumAttribute,
    writeFloatImageAttribute,
    writeDoubleAttribute,
    writeDoubleSpectrumAttribute,
    writeDoubleImageAttribute,
    writeStateAttribute,
    writeStateSpectrumAttribute,
    writeStateImageAttribute,
    writeEnumAttribute,
    writeEnumSpectrumAttribute,
    writeEnumImageAttribute,

    -- * Commands
    CommandName (CommandName),
    commandInVoidOutVoid,
    CommandData (..),
    commandInOutGeneric,
    commandInEnumOutGeneric,
    commandInGenericOutEnum,
    commandListQuery,
    commandQuery,
    CommandInfo (..),

    -- * Properties
    PropertyName (..),
    getDeviceProperties,
    putDeviceProperties,
    deleteDeviceProperties,
    HaskellTangoDevState (..),

    -- * Database proxy
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
import Control.Applicative.Free (Ap, liftAp, runAp, runAp_)
import Control.Exception (Exception, throw)
import Control.Monad (fail, forM_, mapM_, void, when, (>>=))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool (False, True), otherwise, (||))
import Data.Char (Char)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)), (/=))
import Data.Foldable (any)
import Data.Function (const, id, ($), (.))
import Data.Functor (Functor, (<$>))
import Data.Int (Int, Int16, Int32, Int64)
import Data.List (drop, head, length, singleton, splitAt)
import Data.Maybe (Maybe (Just, Nothing), listToMaybe, maybe)
import Data.Ord (max, (>))
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import Data.Text (Text, intercalate, isPrefixOf, null, pack, splitOn, strip, unpack)
import Data.Text.IO (putStrLn)
import Data.Traversable (traverse)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Storable)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (IO, print)
import Tango.Raw.Common
  ( DatabaseProxyPtr,
    DevFailed (DevFailed, devFailedDesc, devFailedOrigin, devFailedReason, devFailedSeverity),
    DeviceProxyPtr,
    HaskellAttrWriteType (Read, ReadWrite),
    HaskellAttributeData (..),
    HaskellAttributeDataList (attributeDataListSequence),
    HaskellAttributeInfoList (HaskellAttributeInfoList, attributeInfoListLength, attributeInfoListSequence),
    HaskellCommandData (..),
    HaskellCommandInfo (..),
    HaskellCommandInfoList (HaskellCommandInfoList, commandInfoLength, commandInfoSequence),
    HaskellDataFormat (..),
    HaskellDataQuality (..),
    HaskellDbData (..),
    HaskellDbDatum (..),
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
    HaskellTangoCommandData (..),
    HaskellTangoDataType (..),
    HaskellTangoDevState (..),
    HaskellTangoPropertyData (..),
    HaskellTangoVarArray (..),
    Timeval (..),
    tango_command_inout,
    tango_command_list_query,
    tango_command_query,
    tango_create_database_proxy,
    tango_create_device_proxy,
    tango_delete_database_proxy,
    tango_delete_device_property,
    tango_delete_device_proxy,
    tango_free_AttributeData,
    tango_free_AttributeInfoList,
    tango_free_CommandData,
    tango_free_CommandInfo,
    tango_free_CommandInfoList,
    tango_free_DbData,
    tango_free_DbDatum,
    tango_free_VarStringArray,
    tango_get_attribute_config,
    tango_get_attribute_list,
    tango_get_device_exported,
    tango_get_device_exported_for_class,
    tango_get_device_property,
    tango_get_object_list,
    tango_get_object_property_list,
    tango_get_property,
    tango_get_timeout_millis,
    tango_lock,
    tango_poll_attribute,
    tango_poll_command,
    tango_put_device_property,
    tango_read_attribute,
    tango_set_timeout_millis,
    tango_stop_poll_attribute,
    tango_stop_poll_command,
    tango_unlock,
    tango_write_attribute,
  )
import qualified Tango.Raw.Common as RawCommon
import Text.Show (Show, show)
import UnliftIO (MonadUnliftIO, bracket)
import qualified UnliftIO
import UnliftIO.Environment (getArgs, getProgName)
import UnliftIO.Foreign (CBool, CDouble, CFloat, CLong, CShort, CULong, CUShort, FunPtr, alloca, castCCharToChar, castPtr, free, new, newArray, newCString, peek, peekArray, peekCString, poke, with, withArray, withCString)
import Prelude (Bounded, Double, Enum (fromEnum, toEnum), Float, Fractional, Integral, Num ((*)), Real, div, error, fromIntegral, realToFrac, undefined)

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

-- | Try to parse a Tango URL like @tango:\/\/host:port\/foo\/bar\/baz@.
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

-- This just looks nicer because not a pointer
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
    with newValue $ \newValuePtr -> with
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
      \newDataPtr -> liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

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
    withArray newValues \newValuesPtr -> with
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
      \newDataPtr -> liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

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
    withArray (imageContent newImage) \newValuesPtr -> with
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
      \newDataPtr -> liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

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
writeRealImageAttribute :: (MonadUnliftIO m, Real i, Fractional i) => DeviceProxy -> AttributeName -> Image i -> m ()
writeRealImageAttribute proxy attributeName newValues = do
  config <- getConfigForAttribute proxy attributeName
  case attributeInfoDataType config of
    HaskellDevFloat ->
      writeFloatImageAttribute proxy attributeName (realToFrac <$> newValues)
    HaskellDevDouble ->
      writeDoubleImageAttribute proxy attributeName (realToFrac <$> newValues)
    _ -> error $ "tried to write real attribute " <> show attributeName <> " but the attribute is not a real type"

-- | Read an attribute irrespective of the concrete integral type. This just uses 'fromIntegral' internally to convert from any integral type. However, we do query the attribute type beforehand, making this two calls instead of just one. If you're really concerned about performance, try to find out the real type of the attribute.
writeRealAttribute :: (MonadUnliftIO m, Fractional i, Real i) => DeviceProxy -> AttributeName -> i -> m ()
writeRealAttribute proxy attributeName newValue = do
  config <- getConfigForAttribute proxy attributeName
  case attributeInfoDataType config of
    HaskellDevFloat ->
      writeFloatAttribute proxy attributeName (realToFrac newValue)
    HaskellDevDouble ->
      writeDoubleAttribute proxy attributeName (realToFrac newValue)
    _ -> error $ "tried to write real attribute " <> show attributeName <> " but the attribute is not a real type"

-- | Read a spectrum attribute irrespective of the concrete real type. This just uses 'realToFrac' internally to convert from any integral type. However, we do query the attribute type beforehand, making this two calls instead of just one. If you're really concerned about performance, try to find out the real type of the attribute.
writeRealSpectrumAttribute :: (MonadUnliftIO m, Fractional i, Real i, Show i) => DeviceProxy -> AttributeName -> [i] -> m ()
writeRealSpectrumAttribute proxy attributeName newValues = do
  config <- getConfigForAttribute proxy attributeName
  case attributeInfoDataType config of
    HaskellDevVarFloatArray ->
      writeFloatSpectrumAttribute proxy attributeName (realToFrac <$> newValues)
    HaskellDevVarDoubleArray ->
      writeDoubleSpectrumAttribute proxy attributeName (realToFrac <$> newValues)
    _ -> error $ "tried to write real attribute " <> show attributeName <> " but the attribute is not a real type"

writeBoolAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Bool -> m ()
writeBoolAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (boolToCBool newValue) HaskellDevBoolean HaskellAttributeDataBoolArray

writeBoolSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Bool] -> m ()
writeBoolSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (boolToCBool <$> newValues) HaskellDevBoolean HaskellAttributeDataBoolArray

writeBoolImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Bool -> m ()
writeBoolImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (boolToCBool <$> newImage) HaskellDevBoolean HaskellAttributeDataBoolArray

writeShortAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Int16 -> m ()
writeShortAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevShort HaskellAttributeDataShortArray

writeShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Int16] -> m ()
writeShortSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevShort HaskellAttributeDataShortArray

writeShortImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Int16 -> m ()
writeShortImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevShort HaskellAttributeDataShortArray

writeUShortAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Word16 -> m ()
writeUShortAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevUShort HaskellAttributeDataUShortArray

writeUShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Word16] -> m ()
writeUShortSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevUShort HaskellAttributeDataUShortArray

writeUShortImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Word16 -> m ()
writeUShortImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevUShort HaskellAttributeDataUShortArray

writeLongAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Int64 -> m ()
writeLongAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevLong HaskellAttributeDataLongArray

writeLongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Int64] -> m ()
writeLongSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevLong HaskellAttributeDataLongArray

writeLongImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Int64 -> m ()
writeLongImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevLong HaskellAttributeDataLongArray

writeULongAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Word64 -> m ()
writeULongAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevULong HaskellAttributeDataULongArray

writeULongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Word64] -> m ()
writeULongSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevULong HaskellAttributeDataULongArray

writeULongImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Word64 -> m ()
writeULongImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevULong HaskellAttributeDataULongArray

writeULong64Attribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Word64 -> m ()
writeULong64Attribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevULong64 HaskellAttributeDataULong64Array

writeULong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Word64] -> m ()
writeULong64SpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevULong64 HaskellAttributeDataULong64Array

writeULong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Word64 -> m ()
writeULong64ImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevULong64 HaskellAttributeDataULong64Array

writeFloatAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Double -> m ()
writeFloatAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (realToFrac newValue) HaskellDevFloat HaskellAttributeDataFloatArray

writeFloatSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Double] -> m ()
writeFloatSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (realToFrac <$> newValues) HaskellDevFloat HaskellAttributeDataFloatArray

writeFloatImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Double -> m ()
writeFloatImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (realToFrac <$> newImage) HaskellDevFloat HaskellAttributeDataFloatArray

writeDoubleAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Double -> m ()
writeDoubleAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (realToFrac newValue) HaskellDevDouble HaskellAttributeDataDoubleArray

writeDoubleSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Double] -> m ()
writeDoubleSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (realToFrac <$> newValues) HaskellDevDouble HaskellAttributeDataDoubleArray

writeDoubleImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Double -> m ()
writeDoubleImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (realToFrac <$> newImage) HaskellDevDouble HaskellAttributeDataDoubleArray

writeStateAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> HaskellTangoDevState -> m ()
writeStateAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName newValue HaskellDevState HaskellAttributeDataStateArray

writeStateSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [HaskellTangoDevState] -> m ()
writeStateSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName newValues HaskellDevState HaskellAttributeDataStateArray

writeStateImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image HaskellTangoDevState -> m ()
writeStateImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName newImage HaskellDevState HaskellAttributeDataStateArray

writeEnumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> t -> m ()
writeEnumAttribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral (fromEnum newValue)) HaskellDevEnum HaskellAttributeDataShortArray

writeEnumSpectrumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> [t] -> m ()
writeEnumSpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral . fromEnum <$> newValues) HaskellDevEnum HaskellAttributeDataShortArray

writeEnumImageAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> Image t -> m ()
writeEnumImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral . fromEnum <$> newImage) HaskellDevEnum HaskellAttributeDataShortArray

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

writeStringSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Text] -> m ()
writeStringSpectrumAttribute (DeviceProxy proxyPtr) (AttributeName attributeName) newValues =
  withCStringText attributeName \attributeNameC ->
    bracket (traverse (newCString . unpack) newValues) (traverse free) \stringPointerList ->
      withArray stringPointerList \stringPointerPtr -> with
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
        \newDataPtr -> liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

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

writeLong64Attribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Int64 -> m ()
writeLong64Attribute proxy attributeName newValue =
  writeScalarAttribute proxy attributeName (fromIntegral newValue) HaskellDevLong64 HaskellAttributeDataLong64Array

writeLong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> [Int64] -> m ()
writeLong64SpectrumAttribute proxy attributeName newValues =
  writeSpectrumAttribute proxy attributeName (fromIntegral <$> newValues) HaskellDevLong64 HaskellAttributeDataLong64Array

writeLong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Image Int64 -> m ()
writeLong64ImageAttribute proxy attributeName newImage =
  writeImageAttribute proxy attributeName (fromIntegral <$> newImage) HaskellDevLong64 HaskellAttributeDataLong64Array

-- | Newtype wrapper to wrap an attribute name
newtype AttributeName = AttributeName Text deriving (Show)

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

readAttributeSimple ::
  (MonadIO m, Storable a, Show a) =>
  (HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray a)) ->
  (HaskellAttributeData -> AtLeastTwo a -> m b) ->
  DeviceProxy ->
  AttributeName ->
  m b
readAttributeSimple extractValue convertValue proxy attributeName = do
  (attributeData, tangoArray) <- readAttributeGeneral (\d -> pure ((d,) <$> extractValue (tangoAttributeData d))) proxy attributeName
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

data TangoValue a = TangoValue
  { tangoValueRead :: a,
    tangoValueWrite :: a
  }
  deriving (Show)

data Image a = Image
  { imageContent :: ![a],
    imageDimX :: !Int,
    imageDimY :: !Int
  }
  deriving (Show, Functor)

-- | Read an attribute irrespective of the concrete integral type. This just uses 'fromIntegral' internally to convert to 'Int'
readIntegralAttribute :: forall m i. (MonadUnliftIO m, Integral i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue i)
readIntegralAttribute = readAttributeSimple' extractIntegral (convertGenericScalar id)

extractIntegral :: (Integral i, MonadUnliftIO m) => HaskellTangoAttributeData -> m (Maybe [i])
extractIntegral (HaskellAttributeDataLongArray a) = do
  arrayElements <- peekArray (fromIntegral (varArrayLength a)) (varArrayValues a)
  pure $ Just (fromIntegral <$> arrayElements)
extractIntegral _ = pure Nothing

-- | Read a spectrum attribute irrespective of the concrete integral element type. This just uses 'fromIntegral' internally
readIntegralSpectrumAttribute :: (MonadUnliftIO m, Integral i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue [i])
readIntegralSpectrumAttribute = readAttributeSimple' extractIntegral convertGenericSpectrum'

-- | Read a spectrum image attribute irrespective of the concrete integral element type. This just uses 'fromIntegral' internally
readIntegralImageAttribute :: (MonadUnliftIO m, Integral i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue (Image i))
readIntegralImageAttribute = readAttributeSimple' extractIntegral convertGenericImage'

extractReal :: (Real i, Fractional i, MonadUnliftIO m) => HaskellTangoAttributeData -> m (Maybe [i])
extractReal (HaskellAttributeDataDoubleArray a) = do
  arrayElements <- peekArray (fromIntegral (varArrayLength a)) (varArrayValues a)
  pure $ Just (realToFrac <$> arrayElements)
extractReal (HaskellAttributeDataFloatArray a) = do
  arrayElements <- peekArray (fromIntegral (varArrayLength a)) (varArrayValues a)
  pure $ Just (realToFrac <$> arrayElements)
extractReal _ = pure Nothing

-- | Read an attribute irrespective of the concrete real type. This just uses 'realToFrac' internally to convert to 'Int'
readRealAttribute :: forall m i. (MonadUnliftIO m, Fractional i, Real i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue i)
readRealAttribute = readAttributeSimple' extractReal (convertGenericScalar id)

-- | Read a spectrum attribute irrespective of the concrete real element type. This just uses 'realToFrac' internally
readRealSpectrumAttribute :: (MonadUnliftIO m, Real i, Fractional i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue [i])
readRealSpectrumAttribute = readAttributeSimple' extractReal convertGenericSpectrum'

-- | Read a spectrum image attribute irrespective of the concrete integral element type. This just uses 'fromIntegral' internally
readRealImageAttribute :: (MonadUnliftIO m, Real i, Fractional i, Show i) => DeviceProxy -> AttributeName -> m (TangoValue (Image i))
readRealImageAttribute = readAttributeSimple' extractReal convertGenericImage'

extractBool :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CBool)
extractBool (HaskellAttributeDataBoolArray a) = Just a
extractBool _ = Nothing

readBoolAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Bool)
readBoolAttribute = readAttributeSimple extractBool (convertGenericScalar cboolToBool)

readBoolSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Bool])
readBoolSpectrumAttribute = readAttributeSimple extractBool (convertGenericSpectrum cboolToBool)

readBoolImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Bool))
readBoolImageAttribute = readAttributeSimple extractBool (convertGenericImage cboolToBool)

-- | Read a string attribute and decode it into a text
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

readShortAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Int16)
readShortAttribute = readAttributeSimple extractShort (convertGenericScalar fromIntegral)

readShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Int16])
readShortSpectrumAttribute = readAttributeSimple extractShort (convertGenericSpectrum fromIntegral)

readShortImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Int16))
readShortImageAttribute = readAttributeSimple extractShort (convertGenericImage fromIntegral)

extractUShort :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CUShort)
extractUShort (HaskellAttributeDataUShortArray a) = Just a
extractUShort _ = Nothing

readUShortAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Word16)
readUShortAttribute = readAttributeSimple extractUShort (convertGenericScalar fromIntegral)

readUShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Word16])
readUShortSpectrumAttribute = readAttributeSimple extractUShort (convertGenericSpectrum fromIntegral)

readUShortImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Word16))
readUShortImageAttribute = readAttributeSimple extractUShort (convertGenericImage fromIntegral)

extractLong :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CLong)
extractLong (HaskellAttributeDataLongArray a) = Just a
extractLong _ = Nothing

readLongAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Int64)
readLongAttribute = readAttributeSimple extractLong (convertGenericScalar fromIntegral)

readLongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Int64])
readLongSpectrumAttribute = readAttributeSimple extractLong (convertGenericSpectrum fromIntegral)

readLongImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Int64))
readLongImageAttribute = readAttributeSimple extractLong (convertGenericImage fromIntegral)

extractULong :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CULong)
extractULong (HaskellAttributeDataULongArray a) = Just a
extractULong _ = Nothing

readULongAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Word64)
readULongAttribute = readAttributeSimple extractULong (convertGenericScalar fromIntegral)

readULongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Word64])
readULongSpectrumAttribute = readAttributeSimple extractULong (convertGenericSpectrum fromIntegral)

readULongImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Word64))
readULongImageAttribute = readAttributeSimple extractULong (convertGenericImage fromIntegral)

extractLong64 :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CLong)
extractLong64 (HaskellAttributeDataLong64Array a) = Just a
extractLong64 _ = Nothing

readLong64Attribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Int64)
readLong64Attribute = readAttributeSimple extractLong64 (convertGenericScalar fromIntegral)

readLong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Int64])
readLong64SpectrumAttribute = readAttributeSimple extractLong64 (convertGenericSpectrum fromIntegral)

readLong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Int64))
readLong64ImageAttribute = readAttributeSimple extractLong64 (convertGenericImage fromIntegral)

extractULong64 :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CULong)
extractULong64 (HaskellAttributeDataULong64Array a) = Just a
extractULong64 _ = Nothing

readULong64Attribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Word64)
readULong64Attribute = readAttributeSimple extractULong64 (convertGenericScalar fromIntegral)

readULong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Word64])
readULong64SpectrumAttribute = readAttributeSimple extractULong64 (convertGenericSpectrum fromIntegral)

readULong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Word64))
readULong64ImageAttribute = readAttributeSimple extractULong64 (convertGenericImage fromIntegral)

extractFloat :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CFloat)
extractFloat (HaskellAttributeDataFloatArray a) = Just a
extractFloat _ = Nothing

readFloatAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Double)
readFloatAttribute = readAttributeSimple extractFloat (convertGenericScalar realToFrac)

readFloatSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Double])
readFloatSpectrumAttribute = readAttributeSimple extractFloat (convertGenericSpectrum realToFrac)

readFloatImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Double))
readFloatImageAttribute = readAttributeSimple extractFloat (convertGenericImage realToFrac)

extractDouble :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CDouble)
extractDouble (HaskellAttributeDataDoubleArray a) = Just a
extractDouble _ = Nothing

readDoubleAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Double)
readDoubleAttribute = readAttributeSimple extractDouble (convertGenericScalar realToFrac)

readDoubleSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Double])
readDoubleSpectrumAttribute = readAttributeSimple extractDouble (convertGenericSpectrum realToFrac)

readDoubleImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Double))
readDoubleImageAttribute = readAttributeSimple extractDouble (convertGenericImage realToFrac)

extractState :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray HaskellTangoDevState)
extractState (HaskellAttributeDataStateArray a) = Just a
extractState _ = Nothing

readStateAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue HaskellTangoDevState)
readStateAttribute = readAttributeSimple extractState (convertGenericScalar id)

readStateSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [HaskellTangoDevState])
readStateSpectrumAttribute = readAttributeSimple extractState (convertGenericSpectrum id)

readStateImageAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image HaskellTangoDevState))
readStateImageAttribute = readAttributeSimple extractState (convertGenericImage id)

extractEnum :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CShort)
extractEnum (HaskellAttributeDataShortArray a) = Just a
extractEnum _ = Nothing

readEnumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (TangoValue t)
readEnumAttribute = readAttributeSimple extractEnum (convertGenericScalar (toEnum . fromIntegral))

readEnumSpectrumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (TangoValue [t])
readEnumSpectrumAttribute = readAttributeSimple extractEnum (convertGenericSpectrum (toEnum . fromIntegral))

readEnumImageAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (TangoValue (Image t))
readEnumImageAttribute = readAttributeSimple extractEnum (convertGenericImage (toEnum . fromIntegral))

newtype CommandName = CommandName Text

instance Show CommandName where
  show (CommandName n) = show (unpack n)

data CommandData
  = CommandVoid
  | CommandBool !Bool
  | CommandShort !Int16
  | CommandUShort !Word16
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

commandInEnumOutGeneric :: (MonadUnliftIO m, Enum t) => DeviceProxy -> CommandName -> t -> m CommandData
commandInEnumOutGeneric proxy commandName in' = commandInOutGeneric proxy commandName (CommandShort $ fromIntegral $ fromEnum in')

commandInGenericOutEnum :: (MonadUnliftIO m, Enum t) => DeviceProxy -> CommandName -> CommandData -> m t
commandInGenericOutEnum proxy commandName in' = do
  result <- commandInOutGeneric proxy commandName in'
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
    attributeInfoEnumLabels :: [Text]
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

newtype PropertyName = PropertyName Text

instance Show PropertyName where
  show (PropertyName x) = show (unpack x)

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

newtype Milliseconds = Milliseconds Int

instance Show Milliseconds where
  show (Milliseconds ms) = show ms <> "ms"

setTimeout :: (MonadUnliftIO m) => DeviceProxy -> Milliseconds -> m ()
setTimeout (DeviceProxy proxy) (Milliseconds ms) = liftIO $ checkResult $ tango_set_timeout_millis proxy (fromIntegral ms)

getTimeout :: (MonadUnliftIO m) => DeviceProxy -> m Milliseconds
getTimeout (DeviceProxy proxy) = liftIO $ with 0 \intPtr -> do
  checkResult (tango_get_timeout_millis proxy intPtr)
  intValue <- peek intPtr
  pure (Milliseconds (fromIntegral intValue))

pollCommand :: (MonadUnliftIO m) => DeviceProxy -> CommandName -> Milliseconds -> m ()
pollCommand (DeviceProxy proxy) (CommandName commandName) (Milliseconds ms) = liftIO $ withCStringText commandName \commandNameC -> checkResult (tango_poll_command proxy commandNameC (fromIntegral ms))

stopPollCommand :: (MonadUnliftIO m) => DeviceProxy -> CommandName -> m ()
stopPollCommand (DeviceProxy proxy) (CommandName commandName) = liftIO $ withCStringText commandName (checkResult . tango_stop_poll_command proxy)

pollAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> Milliseconds -> m ()
pollAttribute (DeviceProxy proxy) (AttributeName attributeName) (Milliseconds ms) = liftIO $ withCStringText attributeName \attributeNameC -> checkResult (tango_poll_attribute proxy attributeNameC (fromIntegral ms))

stopPollAttribute :: (MonadUnliftIO m) => DeviceProxy -> AttributeName -> m ()
stopPollAttribute (DeviceProxy proxy) (AttributeName attributeName) = liftIO $ withCStringText attributeName (checkResult . tango_stop_poll_attribute proxy)

data DisplayLevel = Operator | Expert deriving (Show, Enum, Bounded, Eq)

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

commandListQuery :: (MonadUnliftIO m) => DeviceProxy -> m [CommandInfo]
commandListQuery (DeviceProxy proxy) = liftIO $ with (HaskellCommandInfoList 0 nullPtr) \infoListPtr -> do
  checkResult (tango_command_list_query proxy infoListPtr)
  infoList <- peek infoListPtr
  converted <- peekArray (fromIntegral (commandInfoLength infoList)) (commandInfoSequence infoList)
  result <- traverse convertCommandInfo converted
  tango_free_CommandInfoList infoListPtr
  pure result

commandQuery :: (MonadUnliftIO m) => DeviceProxy -> CommandName -> m CommandInfo
commandQuery (DeviceProxy proxy) (CommandName commandName) = liftIO $ withCStringText commandName \commandNamePtr -> alloca \commandInfoPtr -> do
  checkResult (tango_command_query proxy commandNamePtr commandInfoPtr)
  commandInfoC <- peek commandInfoPtr
  result <- convertCommandInfo commandInfoC
  tango_free_CommandInfo commandInfoPtr
  pure result

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
