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
-- Properties
--
-- The property retrieval API for Tango is elaborate, supporting different data types. We condensed this down to
-- retrieving lists of strings. Conversion needs to happen on the Haskell side for now.
module Tango.Client
  ( withDeviceProxy,
    checkResult,
    getConfigsForAttributes,
    AttributeInfo (..),
    commandInVoidOutVoid,
    getDeviceProperties,
    putDeviceProperties,
    deleteDeviceProperties,
    PropertyName (..),
    HaskellDevFailed (HaskellDevFailed),
    CommandData (..),
    TangoValue (TangoValue),
    commandInOutGeneric,
    commandInEnumOutGeneric,
    commandInGenericOutEnum,
    Image (Image, imageContent, imageDimX, imageDimY),
    devFailedDesc,
    throwTangoException,
    devFailedReason,
    devFailedOrigin,
    devFailedSeverity,
    newDeviceProxy,
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
    tangoUrlFromText,
    DeviceProxyPtr,
    CommandName (CommandName),
    TangoUrl,
    AttributeName (AttributeName),
    HaskellTangoDevState (..),
    TangoException (TangoException),
  )
where

import Control.Applicative (Applicative, pure, (<*>))
import Control.Applicative.Free (Ap, liftAp, runAp, runAp_)
import Control.Exception (Exception, bracket, throw)
import Control.Monad (fail, forM_, mapM_, void, when, (>>=))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool (False, True), otherwise, (||))
import Data.Char (Char)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)), (/=))
import Data.Foldable (any)
import Data.Function (id, ($), (.))
import Data.Functor (Functor, (<$>))
import Data.Int (Int, Int16, Int32, Int64)
import Data.List (drop, length, singleton, splitAt)
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
  ( DeviceProxyPtr,
    HaskellAttrWriteType (Read, ReadWrite),
    HaskellAttributeData (..),
    HaskellAttributeDataList (attributeDataListSequence),
    HaskellAttributeInfoList (HaskellAttributeInfoList, attributeInfoListLength, attributeInfoListSequence),
    HaskellCommandData (..),
    HaskellDataFormat (..),
    HaskellDataQuality (..),
    HaskellDbData (..),
    HaskellDbDatum (..),
    HaskellDevFailed (HaskellDevFailed, devFailedDesc, devFailedOrigin, devFailedReason, devFailedSeverity),
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
    tango_create_device_proxy,
    tango_delete_device_property,
    tango_delete_device_proxy,
    tango_free_AttributeData,
    tango_free_AttributeInfoList,
    tango_free_CommandData,
    tango_free_DbData,
    tango_free_DbDatum,
    tango_get_attribute_config,
    tango_get_device_property,
    tango_get_property,
    tango_get_timeout_millis,
    tango_put_device_property,
    tango_read_attribute,
    tango_set_timeout_millis,
    tango_throw_exception,
    tango_write_attribute,
  )
import qualified Tango.Raw.Common as RawCommon
import Text.Show (Show, show)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO
import UnliftIO.Environment (getArgs, getProgName)
import UnliftIO.Foreign (CBool, CDouble, CFloat, CLong, CShort, CULong, CUShort, FunPtr, alloca, castCCharToChar, castPtr, free, new, newArray, newCString, peek, peekArray, peekCString, poke, with, withArray, withCString)
import Prelude (Double, Enum (fromEnum, toEnum), Float, Integral, Num ((*)), div, error, fromIntegral, realToFrac, undefined)

newtype TangoException = TangoException [HaskellDevFailed Text] deriving (Show)

instance Exception TangoException

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
    formattedStackItems :: [HaskellDevFailed Text] <- traverse (traverse peekCStringText) stackItems
    throw (TangoException formattedStackItems)

newtype TangoUrl = TangoUrl Text

tangoUrlFromText :: Text -> Either Text TangoUrl
tangoUrlFromText url =
  let tangoUrlFromText' url' =
        let urlComponents = splitOn "/" url'
         in if length urlComponents /= 3 || any null urlComponents
              then Left $ "\"" <> url <> "\" is not a valid tango URL: has to be of the form \"[tango://host:port/]domain/family/member\""
              else Right (TangoUrl url)
   in tangoUrlFromText' $
        if "tango://" `isPrefixOf` url
          then intercalate "/" $ drop 3 (splitOn "/" url)
          else url

-- This just looks nicer because not a pointer
type DeviceProxy = DeviceProxyPtr

boolToCBool :: Bool -> CBool
boolToCBool True = 1
boolToCBool False = 0

cboolToBool :: CBool -> Bool
cboolToBool x
  | x > 0 = True
  | otherwise = False

newDeviceProxy :: forall m. (MonadUnliftIO m) => TangoUrl -> m DeviceProxy
newDeviceProxy (TangoUrl url) = do
  alloca $ \proxyPtrPtr -> do
    withCString (unpack url) $ \proxyName -> do
      liftIO $ checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
      liftIO $ peek proxyPtrPtr

withDeviceProxy :: forall m a. (MonadUnliftIO m) => TangoUrl -> (DeviceProxy -> m a) -> m a
withDeviceProxy (TangoUrl proxyAddress) =
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

writeScalarAttribute :: (MonadUnliftIO m, Storable tangoType) => DeviceProxyPtr -> AttributeName -> tangoType -> HaskellTangoDataType -> (HaskellTangoVarArray tangoType -> HaskellTangoAttributeData) -> m ()
writeScalarAttribute proxyPtr (AttributeName attributeName) newValue tangoType intract = do
  withCString (unpack attributeName) $ \attributeNameC ->
    with newValue $ \newValuePtr -> with
      ( HaskellAttributeData
          { dataFormat = HaskellScalar,
            dataQuality = HaskellValid,
            nbRead = 0,
            name = attributeNameC,
            dimX = 1,
            dimY = 1,
            timeStamp = Timeval 0 0,
            dataType = tangoType,
            tangoAttributeData = intract (HaskellTangoVarArray 1 newValuePtr)
          }
      )
      $ \newDataPtr -> liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

writeSpectrumAttribute :: (MonadUnliftIO m, Storable tangoType) => DeviceProxyPtr -> AttributeName -> [tangoType] -> HaskellTangoDataType -> (HaskellTangoVarArray tangoType -> HaskellTangoAttributeData) -> m ()
writeSpectrumAttribute proxyPtr (AttributeName attributeName) newValues tangoType intract =
  withCString (unpack attributeName) $ \attributeNameC ->
    withArray newValues \newValuesPtr -> with
      ( HaskellAttributeData
          { dataFormat = HaskellSpectrum,
            dataQuality = HaskellValid,
            nbRead = 0,
            name = attributeNameC,
            dimX = fromIntegral (length newValues),
            dimY = 1,
            timeStamp = Timeval 0 0,
            dataType = tangoType,
            tangoAttributeData = intract (HaskellTangoVarArray (fromIntegral (length newValues)) newValuesPtr)
          }
      )
      $ \newDataPtr -> liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

writeImageAttribute :: (MonadUnliftIO m, Storable tangoType) => DeviceProxyPtr -> AttributeName -> Image tangoType -> HaskellTangoDataType -> (HaskellTangoVarArray tangoType -> HaskellTangoAttributeData) -> m ()
writeImageAttribute proxyPtr (AttributeName attributeName) newImage tangoType intract =
  withCString (unpack attributeName) $ \attributeNameC ->
    withArray (imageContent newImage) \newValuesPtr -> with
      ( HaskellAttributeData
          { dataFormat = HaskellImage,
            dataQuality = HaskellValid,
            nbRead = 0,
            name = attributeNameC,
            dimX = fromIntegral (imageDimX newImage),
            dimY = fromIntegral (imageDimY newImage),
            timeStamp = Timeval 0 0,
            dataType = tangoType,
            tangoAttributeData = intract (HaskellTangoVarArray (fromIntegral (length (imageContent newImage))) newValuesPtr)
          }
      )
      $ \newDataPtr -> liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

writeBoolAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Bool -> m ()
writeBoolAttribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (boolToCBool newValue) HaskellDevBoolean HaskellAttributeDataBoolArray

writeBoolSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Bool] -> m ()
writeBoolSpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (boolToCBool <$> newValues) HaskellDevBoolean HaskellAttributeDataBoolArray

writeBoolImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Bool -> m ()
writeBoolImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (boolToCBool <$> newImage) HaskellDevBoolean HaskellAttributeDataBoolArray

writeShortAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Int16 -> m ()
writeShortAttribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (fromIntegral newValue) HaskellDevShort HaskellAttributeDataShortArray

writeShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Int16] -> m ()
writeShortSpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (fromIntegral <$> newValues) HaskellDevShort HaskellAttributeDataShortArray

writeShortImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Int16 -> m ()
writeShortImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (fromIntegral <$> newImage) HaskellDevShort HaskellAttributeDataShortArray

writeUShortAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Word16 -> m ()
writeUShortAttribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (fromIntegral newValue) HaskellDevUShort HaskellAttributeDataUShortArray

writeUShortSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Word16] -> m ()
writeUShortSpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (fromIntegral <$> newValues) HaskellDevUShort HaskellAttributeDataUShortArray

writeUShortImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Word16 -> m ()
writeUShortImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (fromIntegral <$> newImage) HaskellDevUShort HaskellAttributeDataUShortArray

writeLongAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Int64 -> m ()
writeLongAttribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (fromIntegral newValue) HaskellDevLong HaskellAttributeDataLongArray

writeLongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Int64] -> m ()
writeLongSpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (fromIntegral <$> newValues) HaskellDevLong HaskellAttributeDataLongArray

writeLongImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Int64 -> m ()
writeLongImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (fromIntegral <$> newImage) HaskellDevLong HaskellAttributeDataLongArray

writeULongAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Word64 -> m ()
writeULongAttribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (fromIntegral newValue) HaskellDevULong HaskellAttributeDataULongArray

writeULongSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Word64] -> m ()
writeULongSpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (fromIntegral <$> newValues) HaskellDevULong HaskellAttributeDataULongArray

writeULongImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Word64 -> m ()
writeULongImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (fromIntegral <$> newImage) HaskellDevULong HaskellAttributeDataULongArray

writeULong64Attribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Word64 -> m ()
writeULong64Attribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (fromIntegral newValue) HaskellDevULong64 HaskellAttributeDataULong64Array

writeULong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Word64] -> m ()
writeULong64SpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (fromIntegral <$> newValues) HaskellDevULong64 HaskellAttributeDataULong64Array

writeULong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Word64 -> m ()
writeULong64ImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (fromIntegral <$> newImage) HaskellDevULong64 HaskellAttributeDataULong64Array

writeFloatAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Double -> m ()
writeFloatAttribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (realToFrac newValue) HaskellDevFloat HaskellAttributeDataFloatArray

writeFloatSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Double] -> m ()
writeFloatSpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (realToFrac <$> newValues) HaskellDevFloat HaskellAttributeDataFloatArray

writeFloatImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Double -> m ()
writeFloatImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (realToFrac <$> newImage) HaskellDevFloat HaskellAttributeDataFloatArray

writeDoubleAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Double -> m ()
writeDoubleAttribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (realToFrac newValue) HaskellDevDouble HaskellAttributeDataDoubleArray

writeDoubleSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Double] -> m ()
writeDoubleSpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (realToFrac <$> newValues) HaskellDevDouble HaskellAttributeDataDoubleArray

writeDoubleImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Double -> m ()
writeDoubleImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (realToFrac <$> newImage) HaskellDevDouble HaskellAttributeDataDoubleArray

writeStateAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> HaskellTangoDevState -> m ()
writeStateAttribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName newValue HaskellDevState HaskellAttributeDataStateArray

writeStateSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [HaskellTangoDevState] -> m ()
writeStateSpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName newValues HaskellDevState HaskellAttributeDataStateArray

writeStateImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image HaskellTangoDevState -> m ()
writeStateImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName newImage HaskellDevState HaskellAttributeDataStateArray

writeEnumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxyPtr -> AttributeName -> t -> m ()
writeEnumAttribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (fromIntegral (fromEnum newValue)) HaskellDevEnum HaskellAttributeDataShortArray

writeEnumSpectrumAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxyPtr -> AttributeName -> [t] -> m ()
writeEnumSpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (fromIntegral . fromEnum <$> newValues) HaskellDevEnum HaskellAttributeDataShortArray

writeEnumImageAttribute :: (MonadUnliftIO m, Enum t) => DeviceProxyPtr -> AttributeName -> Image t -> m ()
writeEnumImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (fromIntegral . fromEnum <$> newImage) HaskellDevEnum HaskellAttributeDataShortArray

writeStringAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Text -> m ()
writeStringAttribute proxyPtr (AttributeName attributeName) newValue =
  withCString (unpack attributeName) $ \attributeNameC -> do
    withCString (unpack newValue) \newValuePtr ->
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

writeStringSpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Text] -> m ()
writeStringSpectrumAttribute proxyPtr (AttributeName attributeName) newValues =
  withCString (unpack attributeName) $ \attributeNameC ->
    UnliftIO.bracket (traverse (newCString . unpack) newValues) (traverse free) \stringPointerList ->
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
        $ \newDataPtr -> liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

writeStringImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Text -> m ()
writeStringImageAttribute proxyPtr (AttributeName attributeName) (Image newImage imageX imageY) =
  withCString (unpack attributeName) $ \attributeNameC ->
    UnliftIO.bracket (traverse (newCString . unpack) newImage) (traverse free) \stringPointerList ->
      withArray stringPointerList \stringPointerPtr -> with
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
        $ \newDataPtr -> liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

writeLong64Attribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Int64 -> m ()
writeLong64Attribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (fromIntegral newValue) HaskellDevLong64 HaskellAttributeDataLong64Array

writeLong64SpectrumAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Int64] -> m ()
writeLong64SpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (fromIntegral <$> newValues) HaskellDevLong64 HaskellAttributeDataLong64Array

writeLong64ImageAttribute :: (MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Int64 -> m ()
writeLong64ImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (fromIntegral <$> newImage) HaskellDevLong64 HaskellAttributeDataLong64Array

-- | Newtype wrapper to wrap an attribute name
newtype AttributeName = AttributeName Text deriving (Show)

readAttributeGeneral :: (MonadIO m) => (HaskellAttributeData -> IO (Maybe a)) -> DeviceProxy -> AttributeName -> m a
readAttributeGeneral extractValue proxyPtr (AttributeName attributeNameHaskell) =
  liftIO $ withCString (unpack attributeNameHaskell) $ \attributeName -> do
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
readAttributeSimple extractValue convertValue proxyPtr attributeName = do
  (attributeData, tangoArray) <- readAttributeGeneral (\d -> pure ((d,) <$> extractValue (tangoAttributeData d))) proxyPtr attributeName
  arrayElements <- peekArray (fromIntegral (varArrayLength tangoArray)) (varArrayValues tangoArray)
  case arrayElements of
    (first : second : rest) -> convertValue attributeData (AtLeastTwo first second rest)
    _ -> error $ "couldn't read attribute " <> show attributeName <> ": expected a value array of length at least two, but got " <> show arrayElements

convertGenericScalar :: (Applicative f) => (a -> b) -> HaskellAttributeData -> AtLeastTwo a -> f (TangoValue b)
convertGenericScalar f _ (AtLeastTwo first second _) = pure (TangoValue (f first) (f second))

convertGenericSpectrum :: (Applicative f) => (a -> b) -> HaskellAttributeData -> AtLeastTwo a -> f (TangoValue [b])
convertGenericSpectrum f (HaskellAttributeData {dimX}) (AtLeastTwo first second remainder) =
  let wholeList = f <$> (first : second : remainder)
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

commandInVoidOutVoid :: (MonadUnliftIO m) => DeviceProxyPtr -> CommandName -> m ()
commandInVoidOutVoid proxyPtr (CommandName commandName) =
  liftIO $
    withCString (unpack commandName) $
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
  UnliftIO.bracket
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

commandInOutGeneric :: (MonadUnliftIO m) => DeviceProxyPtr -> CommandName -> CommandData -> m CommandData
commandInOutGeneric proxyPtr (CommandName commandName) in' =
  liftIO $
    withCString (unpack commandName) $
      \commandNamePtr ->
        withRawCommandData in' \commandDataInPtr ->
          with (RawCommon.HaskellCommandData RawCommon.HaskellDevVoid RawCommon.HaskellCommandVoid) $ \commandDataOutPtr -> do
            checkResult $ tango_command_inout proxyPtr commandNamePtr commandDataInPtr commandDataOutPtr
            outValue <- peek commandDataOutPtr
            result <- fromRawCommandData outValue
            case result of
              Nothing -> error "couldn't convert the command out value"
              Just result' -> pure result'

commandInEnumOutGeneric :: (MonadUnliftIO m, Enum t) => DeviceProxyPtr -> CommandName -> t -> m CommandData
commandInEnumOutGeneric proxyPtr commandName in' = commandInOutGeneric proxyPtr commandName (CommandShort $ fromIntegral $ fromEnum in')

commandInGenericOutEnum :: (MonadUnliftIO m, Enum t) => DeviceProxyPtr -> CommandName -> CommandData -> m t
commandInGenericOutEnum proxyPtr commandName in' = do
  result <- commandInOutGeneric proxyPtr commandName in'
  case result of
    CommandShort s -> pure (toEnum (fromIntegral s))
    _ -> error ("command " <> show commandName <> " was supposed to return a short (for enums), but returned " <> show result)

throwTangoException :: (MonadIO m) => Text -> m ()
throwTangoException desc = do
  str <- newCString (unpack desc)
  liftIO $ tango_throw_exception str

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

convertAttributeInfo :: RawCommon.HaskellAttributeInfo -> IO AttributeInfo
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

getConfigsForAttributes :: DeviceProxy -> [AttributeName] -> IO [AttributeInfo]
getConfigsForAttributes deviceProxyPtr attributeNames = do
  let attributeNameToCString :: AttributeName -> IO CString
      attributeNameToCString (AttributeName t) = newCString (unpack t)
  bracket (traverse attributeNameToCString attributeNames) (traverse free) \cstringList ->
    withArray cstringList \cstringPtr ->
      with (HaskellTangoVarArray (fromIntegral (length attributeNames)) cstringPtr) \varArrayPtr ->
        with (HaskellAttributeInfoList 0 nullPtr) \outputPtr ->
          bracket
            (checkResult (tango_get_attribute_config deviceProxyPtr varArrayPtr outputPtr))
            (\_ -> tango_free_AttributeInfoList outputPtr)
            \_ -> do
              outputPeeked <- peek outputPtr
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
getDeviceProperties proxyPtr names =
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
putDeviceProperties proxyPtr namesAndValues =
  UnliftIO.bracket
    (traverse nameAndValueToDbDatum namesAndValues)
    (void . traverse freeDbDatum)
    \dbDatumPtrListIn ->
      withArray dbDatumPtrListIn \dbDatumPtrIn ->
        liftIO $
          with
            (HaskellDbData (fromIntegral (length namesAndValues)) dbDatumPtrIn)
            (checkResult . liftIO . tango_put_device_property proxyPtr)

deleteDeviceProperties :: forall m. (MonadUnliftIO m) => DeviceProxy -> [PropertyName] -> m ()
deleteDeviceProperties proxyPtr names =
  let initialize :: m [HaskellDbDatum]
      initialize = traverse nameToDbDatum names
      destroy :: [HaskellDbDatum] -> m ()
      destroy = void . traverse freeDbDatum
   in UnliftIO.bracket
        initialize
        destroy
        \dbDatumPtrListIn ->
          withArray dbDatumPtrListIn \dbDatumPtrIn ->
            liftIO $
              with
                (HaskellDbData (fromIntegral (length names)) dbDatumPtrIn)
                (checkResult . liftIO . tango_delete_device_property proxyPtr)
