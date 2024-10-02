{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Tango.Client
  ( withDeviceProxy,
    checkResult,
    getConfigsForAttributes,
    AttributeInfo (..),
    commandInOutVoid,
    HaskellDevFailed (HaskellDevFailed),
    TangoValue (TangoValue),
    Image (Image, imageContent, imageDimX, imageDimY),
    devFailedDesc,
    throwTangoException,
    devFailedReason,
    devFailedOrigin,
    devFailedSeverity,
    newDeviceProxy,
    readStateAttribute,
    readStateSpectrumAttribute,
    readStateImageAttribute,
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
    readEnumAttribute,
    readEnumSpectrumAttribute,
    readEnumImageAttribute,
    writeLong64Attribute,
    writeLong64SpectrumAttribute,
    writeLong64ImageAttribute,
    writeDoubleAttribute,
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
import Data.Bool (Bool, (||))
import Data.Char (Char)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)), (/=))
import Data.Foldable (any)
import Data.Function (id, ($), (.))
import Data.Functor (Functor, (<$>))
import Data.Int (Int, Int16, Int32, Int64)
import Data.List (drop, length, singleton, splitAt)
import Data.Maybe (Maybe (Just, Nothing), listToMaybe, maybe)
import Data.Ord (max)
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import Data.Text (Text, intercalate, isPrefixOf, null, pack, splitOn, strip, unpack)
import Data.Text.IO (putStrLn)
import Data.Traversable (traverse)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Storable, free)
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
    HaskellDevFailed (HaskellDevFailed, devFailedDesc, devFailedOrigin, devFailedReason, devFailedSeverity),
    HaskellDispLevel,
    HaskellErrorStack (errorStackLength, errorStackSequence),
    HaskellTangoAttributeData (HaskellAttributeDataBoolArray, HaskellAttributeDataDoubleArray, HaskellAttributeDataFloatArray, HaskellAttributeDataLong64Array, HaskellAttributeDataLongArray, HaskellAttributeDataShortArray, HaskellAttributeDataStateArray, HaskellAttributeDataStringArray, HaskellAttributeDataULong64Array, HaskellAttributeDataULongArray, HaskellAttributeDataUShortArray),
    HaskellTangoCommandData (..),
    HaskellTangoDataType (..),
    HaskellTangoDevState (..),
    HaskellTangoVarArray (..),
    Timeval (..),
    tango_command_inout,
    tango_create_device_proxy,
    tango_delete_device_proxy,
    tango_free_AttributeData,
    tango_free_AttributeInfoList,
    tango_free_CommandData,
    tango_get_attribute_config,
    tango_get_timeout_millis,
    tango_read_attribute,
    tango_set_timeout_millis,
    tango_throw_exception,
    tango_write_attribute,
  )
import qualified Tango.Raw.Common as RawCommon
import Text.Show (Show, show)
import qualified UnliftIO
import UnliftIO.Environment (getArgs, getProgName)
import UnliftIO.Foreign (CBool, CDouble, CFloat, CLong, CShort, CULong, CUShort, FunPtr, alloca, castPtr, newCString, peek, peekArray, peekCString, poke, with, withArray, withCString)
import Prelude (Double, Enum (fromEnum, toEnum), Float, Integral, Num ((*)), div, error, fromIntegral, realToFrac, undefined)

newtype TangoException = TangoException [HaskellDevFailed Text] deriving (Show)

instance Exception TangoException

peekCStringText :: (UnliftIO.MonadUnliftIO m) => CString -> m Text
peekCStringText x = do
  result <- liftIO (peekCString x)
  pure (pack result)

peekCStringArrayText :: (UnliftIO.MonadUnliftIO m, Integral i) => i -> Ptr CString -> m [Text]
peekCStringArrayText len x = do
  ptrList <- liftIO $ peekArray (fromIntegral len) x
  traverse peekCStringText ptrList

checkResult :: (UnliftIO.MonadUnliftIO m) => m (Ptr HaskellErrorStack) -> m ()
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

newDeviceProxy :: forall m. (UnliftIO.MonadUnliftIO m) => TangoUrl -> m DeviceProxy
newDeviceProxy (TangoUrl url) = do
  alloca $ \proxyPtrPtr -> do
    withCString (unpack url) $ \proxyName -> do
      liftIO $ checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
      liftIO $ peek proxyPtrPtr

withDeviceProxy :: forall m a. (UnliftIO.MonadUnliftIO m) => TangoUrl -> (DeviceProxy -> m a) -> m a
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

writeScalarAttribute :: (UnliftIO.MonadUnliftIO m, Storable tangoType) => DeviceProxyPtr -> AttributeName -> tangoType -> HaskellTangoDataType -> (HaskellTangoVarArray tangoType -> HaskellTangoAttributeData) -> m ()
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

writeSpectrumAttribute :: (UnliftIO.MonadUnliftIO m, Storable tangoType) => DeviceProxyPtr -> AttributeName -> [tangoType] -> HaskellTangoDataType -> (HaskellTangoVarArray tangoType -> HaskellTangoAttributeData) -> m ()
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

writeImageAttribute :: (UnliftIO.MonadUnliftIO m, Storable tangoType) => DeviceProxyPtr -> AttributeName -> Image tangoType -> HaskellTangoDataType -> (HaskellTangoVarArray tangoType -> HaskellTangoAttributeData) -> m ()
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

writeLong64Attribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Int64 -> m ()
writeLong64Attribute proxyPtr attributeName newValue =
  writeScalarAttribute proxyPtr attributeName (fromIntegral newValue) HaskellDevLong64 HaskellAttributeDataLongArray

writeLong64SpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> [Int64] -> m ()
writeLong64SpectrumAttribute proxyPtr attributeName newValues =
  writeSpectrumAttribute proxyPtr attributeName (fromIntegral <$> newValues) HaskellDevLong64 HaskellAttributeDataLongArray

writeLong64ImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Image Int64 -> m ()
writeLong64ImageAttribute proxyPtr attributeName newImage =
  writeImageAttribute proxyPtr attributeName (fromIntegral <$> newImage) HaskellDevLong64 HaskellAttributeDataLongArray

writeDoubleAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> AttributeName -> Double -> m ()
writeDoubleAttribute proxyPtr attributeName newValue = do
  writeScalarAttribute proxyPtr attributeName (realToFrac newValue) HaskellDevDouble HaskellAttributeDataDoubleArray

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

readBoolAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Bool)
readBoolAttribute = readAttributeSimple extractBool (convertGenericScalar (/= 0))

readBoolSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Bool])
readBoolSpectrumAttribute = readAttributeSimple extractBool (convertGenericSpectrum (/= 0))

readBoolImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Bool))
readBoolImageAttribute = readAttributeSimple extractBool (convertGenericImage (/= 0))

-- | Read a string attribute and decode it into a text
readStringAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Text)
readStringAttribute = readAttributeSimple extract convert
  where
    extract (HaskellAttributeDataStringArray a) = Just a
    extract _ = Nothing
    convert _ (AtLeastTwo read write []) = TangoValue <$> peekCStringText read <*> peekCStringText write
    convert _ _ = error "expected a read and a write value for attribute, got more elements"

-- | Read a string spectrum (array/list) attribute and decode it into a text
readStringSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Text])
readStringSpectrumAttribute = readAttributeSimple extract convert
  where
    extract (HaskellAttributeDataStringArray a) = Just a
    extract _ = Nothing
    convert (HaskellAttributeData {dimX}) (AtLeastTwo first second remainder) = do
      wholeList <- traverse peekCStringText (first : second : remainder)
      let (readValue, writeValue) = splitAt (fromIntegral dimX) wholeList
      pure (TangoValue readValue writeValue)

-- | Read a string image attribute and decode it into a text
readStringImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Text))
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

readShortAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Int16)
readShortAttribute = readAttributeSimple extractShort (convertGenericScalar fromIntegral)

readShortSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Int16])
readShortSpectrumAttribute = readAttributeSimple extractShort (convertGenericSpectrum fromIntegral)

readShortImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Int16))
readShortImageAttribute = readAttributeSimple extractShort (convertGenericImage fromIntegral)

extractUShort :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CUShort)
extractUShort (HaskellAttributeDataUShortArray a) = Just a
extractUShort _ = Nothing

readUShortAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Word16)
readUShortAttribute = readAttributeSimple extractUShort (convertGenericScalar fromIntegral)

readUShortSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Word16])
readUShortSpectrumAttribute = readAttributeSimple extractUShort (convertGenericSpectrum fromIntegral)

readUShortImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Word16))
readUShortImageAttribute = readAttributeSimple extractUShort (convertGenericImage fromIntegral)

extractLong :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CLong)
extractLong (HaskellAttributeDataLongArray a) = Just a
extractLong _ = Nothing

readLongAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Int64)
readLongAttribute = readAttributeSimple extractLong (convertGenericScalar fromIntegral)

readLongSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Int64])
readLongSpectrumAttribute = readAttributeSimple extractLong (convertGenericSpectrum fromIntegral)

readLongImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Int64))
readLongImageAttribute = readAttributeSimple extractLong (convertGenericImage fromIntegral)

extractULong :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CULong)
extractULong (HaskellAttributeDataULongArray a) = Just a
extractULong _ = Nothing

readULongAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Word64)
readULongAttribute = readAttributeSimple extractULong (convertGenericScalar fromIntegral)

readULongSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Word64])
readULongSpectrumAttribute = readAttributeSimple extractULong (convertGenericSpectrum fromIntegral)

readULongImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Word64))
readULongImageAttribute = readAttributeSimple extractULong (convertGenericImage fromIntegral)

extractLong64 :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CLong)
extractLong64 (HaskellAttributeDataLong64Array a) = Just a
extractLong64 _ = Nothing

readLong64Attribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Int64)
readLong64Attribute = readAttributeSimple extractLong64 (convertGenericScalar fromIntegral)

readLong64SpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Int64])
readLong64SpectrumAttribute = readAttributeSimple extractLong64 (convertGenericSpectrum fromIntegral)

readLong64ImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Int64))
readLong64ImageAttribute = readAttributeSimple extractLong64 (convertGenericImage fromIntegral)

extractULong64 :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CULong)
extractULong64 (HaskellAttributeDataULong64Array a) = Just a
extractULong64 _ = Nothing

readULong64Attribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Word64)
readULong64Attribute = readAttributeSimple extractULong64 (convertGenericScalar fromIntegral)

readULong64SpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Word64])
readULong64SpectrumAttribute = readAttributeSimple extractULong64 (convertGenericSpectrum fromIntegral)

readULong64ImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Word64))
readULong64ImageAttribute = readAttributeSimple extractULong64 (convertGenericImage fromIntegral)

extractFloat :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CFloat)
extractFloat (HaskellAttributeDataFloatArray a) = Just a
extractFloat _ = Nothing

readFloatAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Double)
readFloatAttribute = readAttributeSimple extractFloat (convertGenericScalar realToFrac)

readFloatSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Double])
readFloatSpectrumAttribute = readAttributeSimple extractFloat (convertGenericSpectrum realToFrac)

readFloatImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Double))
readFloatImageAttribute = readAttributeSimple extractFloat (convertGenericImage realToFrac)

extractDouble :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CDouble)
extractDouble (HaskellAttributeDataDoubleArray a) = Just a
extractDouble _ = Nothing

readDoubleAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue Double)
readDoubleAttribute = readAttributeSimple extractDouble (convertGenericScalar realToFrac)

readDoubleSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [Double])
readDoubleSpectrumAttribute = readAttributeSimple extractDouble (convertGenericSpectrum realToFrac)

readDoubleImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image Double))
readDoubleImageAttribute = readAttributeSimple extractDouble (convertGenericImage realToFrac)

extractState :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray HaskellTangoDevState)
extractState (HaskellAttributeDataStateArray a) = Just a
extractState _ = Nothing

readStateAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue HaskellTangoDevState)
readStateAttribute = readAttributeSimple extractState (convertGenericScalar id)

readStateSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue [HaskellTangoDevState])
readStateSpectrumAttribute = readAttributeSimple extractState (convertGenericSpectrum id)

readStateImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (TangoValue (Image HaskellTangoDevState))
readStateImageAttribute = readAttributeSimple extractState (convertGenericImage id)

extractEnum :: HaskellTangoAttributeData -> Maybe (HaskellTangoVarArray CShort)
extractEnum (HaskellAttributeDataShortArray a) = Just a
extractEnum _ = Nothing

readEnumAttribute :: (UnliftIO.MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (TangoValue t)
readEnumAttribute = readAttributeSimple extractEnum (convertGenericScalar (toEnum . fromIntegral))

readEnumSpectrumAttribute :: (UnliftIO.MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (TangoValue [t])
readEnumSpectrumAttribute = readAttributeSimple extractEnum (convertGenericSpectrum (toEnum . fromIntegral))

readEnumImageAttribute :: (UnliftIO.MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (TangoValue (Image t))
readEnumImageAttribute = readAttributeSimple extractEnum (convertGenericImage (toEnum . fromIntegral))

newtype CommandName = CommandName Text

commandInOutVoid :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> CommandName -> m ()
commandInOutVoid proxyPtr (CommandName commandName) =
  liftIO $
    withCString (unpack commandName) $
      \commandNamePtr ->
        with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \commandDataInPtr -> with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \commandDataOutPtr ->
          checkResult $ tango_command_inout proxyPtr commandNamePtr commandDataInPtr commandDataOutPtr

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
