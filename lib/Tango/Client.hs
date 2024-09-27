{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tango.Client
  ( withDeviceProxy,
    checkResult,
    writeLong64Attribute,
    writeDoubleAttribute,
    getConfigsForAttributes,
    AttributeInfo (..),
    commandInOutVoid,
    HaskellDevFailed (HaskellDevFailed),
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
    tangoUrlFromText,
    DeviceProxyPtr,
    CommandName (CommandName),
    TangoUrl,
    AttributeName (AttributeName),
    HaskellTangoDevState (..),
    TangoException (TangoException),
  )
where

import Control.Applicative (pure)
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
import Data.Function (($), (.))
import Data.Functor (Functor, (<$>))
import Data.Int (Int, Int16, Int32, Int64)
import Data.List (drop, length, singleton)
import Data.Maybe (Maybe (Just, Nothing), listToMaybe, maybe)
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import Data.Text (Text, intercalate, isPrefixOf, null, pack, splitOn, strip, unpack)
import Data.Text.IO (putStrLn)
import Data.Traversable (traverse)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (free)
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
import qualified Tango.Raw.Common as CommonRaw
import qualified Tango.Raw.Common as RawCommon
import Text.Show (Show, show)
import qualified UnliftIO
import UnliftIO.Environment (getArgs, getProgName)
import UnliftIO.Foreign (CDouble, CLong, FunPtr, alloca, castPtr, newCString, peek, peekArray, peekCString, poke, with, withArray, withCString)
import Prelude (Double, Enum (fromEnum, toEnum), Float, Integral, Num ((*)), error, fromIntegral, realToFrac, undefined)

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

writeLong64Attribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> Int64 -> m ()
writeLong64Attribute proxyPtr attributeName newValue = do
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

writeDoubleAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> Double -> m ()
writeDoubleAttribute proxyPtr attributeName newValue = do
  withCString (unpack attributeName) $ \attributeNameC ->
    with (realToFrac newValue) $ \newValuePtr -> with
      ( HaskellAttributeData
          { dataFormat = HaskellScalar,
            dataQuality = HaskellValid,
            nbRead = 0,
            name = attributeNameC,
            dimX = 1,
            dimY = 1,
            timeStamp = Timeval 0 0,
            dataType = HaskellDevDouble,
            tangoAttributeData = HaskellAttributeDataDoubleArray (HaskellTangoVarArray 1 newValuePtr)
          }
      )
      $ \newDataPtr ->
        liftIO $ void (tango_write_attribute proxyPtr newDataPtr)

-- | Newtype wrapper to wrap an attribute name
newtype AttributeName = AttributeName Text

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

readAttributeSimple :: (MonadIO m) => (HaskellTangoAttributeData -> IO (Maybe a)) -> DeviceProxy -> AttributeName -> m a
readAttributeSimple extractValue = readAttributeGeneral (extractValue . tangoAttributeData)

data Image a = Image
  { imageContent :: [a],
    imageDimX :: Int,
    imageDimY :: Int
  }
  deriving (Show)

-- | Read a string attribute and decode it into a text
readStringAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Text
readStringAttribute = readAttributeSimple extract
  where
    extract (HaskellAttributeDataStringArray (HaskellTangoVarArray {varArrayValues})) = do
      firstString <- peek varArrayValues
      result <- peekCStringText firstString
      pure (Just result)
    extract _ = pure Nothing

-- | Read a string spectrum (array/list) attribute and decode it into a text
readStringSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Text]
readStringSpectrumAttribute = readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataStringArray (HaskellTangoVarArray {varArrayValues}) -> do
          haskellStringList <- peekCStringArrayText (dimX a) varArrayValues
          pure (Just haskellStringList)
        _ -> pure Nothing

-- | Read a string image attribute and decode it into a text
readStringImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Text)
readStringImageAttribute = readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataStringArray (HaskellTangoVarArray {varArrayValues}) -> do
          haskellStringList <- peekCStringArrayText (dimX a * dimY a) varArrayValues
          pure $ Just $ Image haskellStringList (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readBoolAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Bool
readBoolAttribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataBoolArray (HaskellTangoVarArray {varArrayValues})) = Just . (/= 0) <$> peek varArrayValues
    extract _ = pure Nothing

readBoolSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Bool]
readBoolSpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataBoolArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just $ (/= 0) <$> arrayResult
        _ -> pure Nothing

readBoolImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Bool)
readBoolImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataBoolArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image ((/= 0) <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readShortAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Int16
readShortAttribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataShortArray (HaskellTangoVarArray {varArrayValues})) = Just . fromIntegral <$> peek varArrayValues
    extract _ = pure Nothing

readShortSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Int16]
readShortSpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataShortArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just $ fromIntegral <$> arrayResult
        _ -> pure Nothing

readShortImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Int16)
readShortImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataShortArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image (fromIntegral <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readUShortAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Word16
readUShortAttribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataUShortArray (HaskellTangoVarArray {varArrayValues})) = Just . fromIntegral <$> peek varArrayValues
    extract _ = pure Nothing

readUShortSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Word16]
readUShortSpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataUShortArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just $ fromIntegral <$> arrayResult
        _ -> pure Nothing

readUShortImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Word16)
readUShortImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataUShortArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image (fromIntegral <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readLongAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Int64
readLongAttribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataLongArray (HaskellTangoVarArray {varArrayValues})) = Just . fromIntegral <$> peek varArrayValues
    extract _ = pure Nothing

readLongSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Int64]
readLongSpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataLongArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just $ fromIntegral <$> arrayResult
        _ -> pure Nothing

readLongImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Int64)
readLongImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataLongArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image (fromIntegral <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readULongAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Word64
readULongAttribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataULongArray (HaskellTangoVarArray {varArrayValues})) = Just . fromIntegral <$> peek varArrayValues
    extract _ = pure Nothing

readULongSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Word64]
readULongSpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataULongArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just $ fromIntegral <$> arrayResult
        _ -> pure Nothing

readULongImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Word64)
readULongImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataULongArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image (fromIntegral <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readLong64Attribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Int64
readLong64Attribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataLong64Array (HaskellTangoVarArray {varArrayValues})) = Just . fromIntegral <$> peek varArrayValues
    extract _ = pure Nothing

readLong64SpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Int64]
readLong64SpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataLong64Array (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just $ fromIntegral <$> arrayResult
        _ -> pure Nothing

readLong64ImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Int64)
readLong64ImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataLong64Array (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image (fromIntegral <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readULong64Attribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Word64
readULong64Attribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataULong64Array (HaskellTangoVarArray {varArrayValues})) = Just . fromIntegral <$> peek varArrayValues
    extract _ = pure Nothing

readULong64SpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Word64]
readULong64SpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataULong64Array (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just $ fromIntegral <$> arrayResult
        _ -> pure Nothing

readULong64ImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Word64)
readULong64ImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataULong64Array (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image (fromIntegral <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readFloatAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Double
readFloatAttribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataFloatArray (HaskellTangoVarArray {varArrayValues})) = Just . realToFrac <$> peek varArrayValues
    extract _ = pure Nothing

readFloatSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Double]
readFloatSpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataFloatArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just $ realToFrac <$> arrayResult
        _ -> pure Nothing

readFloatImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Double)
readFloatImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataFloatArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image (realToFrac <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readDoubleAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Double
readDoubleAttribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataDoubleArray (HaskellTangoVarArray {varArrayValues})) = Just . realToFrac <$> peek varArrayValues
    extract _ = pure Nothing

readDoubleSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [Double]
readDoubleSpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataDoubleArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just $ realToFrac <$> arrayResult
        _ -> pure Nothing

readDoubleImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image Double)
readDoubleImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataDoubleArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image (realToFrac <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

-- | Read a state attribute
readStateAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m HaskellTangoDevState
readStateAttribute = readAttributeSimple extract
  where
    extract (HaskellAttributeDataStateArray (HaskellTangoVarArray {varArrayValues})) = do
      firstValue <- peek varArrayValues
      pure (Just firstValue)
    extract _ = pure Nothing

readStateSpectrumAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m [HaskellTangoDevState]
readStateSpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataStateArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just arrayResult
        _ -> pure Nothing

readStateImageAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m (Image HaskellTangoDevState)
readStateImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataStateArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image arrayResult (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

readEnumAttribute :: (UnliftIO.MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m t
readEnumAttribute =
  readAttributeSimple extract
  where
    extract (HaskellAttributeDataShortArray (HaskellTangoVarArray {varArrayValues})) = do
      v <- peek varArrayValues
      pure (Just (toEnum (fromIntegral v)))
    extract _ = pure Nothing

readEnumSpectrumAttribute :: (UnliftIO.MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m [t]
readEnumSpectrumAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataShortArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a)) varArrayValues
          pure $ Just (toEnum . fromIntegral <$> arrayResult)
        _ -> pure Nothing

readEnumImageAttribute :: (UnliftIO.MonadUnliftIO m, Enum t) => DeviceProxy -> AttributeName -> m (Image t)
readEnumImageAttribute =
  readAttributeGeneral extract
  where
    extract a =
      case tangoAttributeData a of
        HaskellAttributeDataShortArray (HaskellTangoVarArray {varArrayValues}) -> do
          arrayResult <- peekArray (fromIntegral (dimX a * dimY a)) varArrayValues
          pure $ Just $ Image (toEnum . fromIntegral <$> arrayResult) (fromIntegral (dimX a)) (fromIntegral (dimY a))
        _ -> pure Nothing

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

convertAttributeInfo :: CommonRaw.HaskellAttributeInfo -> IO AttributeInfo
convertAttributeInfo ai = do
  description <- peekCStringText (CommonRaw.attributeInfoDescription ai)
  label <- peekCStringText (CommonRaw.attributeInfoLabel ai)
  unit <- peekCStringText (CommonRaw.attributeInfoUnit ai)
  standardUnit <- peekCStringText (CommonRaw.attributeInfoStandardUnit ai)
  displayUnit <- peekCStringText (CommonRaw.attributeInfoDisplayUnit ai)
  format <- peekCStringText (CommonRaw.attributeInfoFormat ai)
  minValue <- peekCStringText (CommonRaw.attributeInfoMinValue ai)
  maxValue <- peekCStringText (CommonRaw.attributeInfoMaxValue ai)
  minAlarm <- peekCStringText (CommonRaw.attributeInfoMinAlarm ai)
  maxAlarm <- peekCStringText (CommonRaw.attributeInfoMaxAlarm ai)
  writableAttrName <- peekCStringText (CommonRaw.attributeInfoWritableAttrName ai)
  enumLabelsList <-
    peekCStringArrayText (CommonRaw.attributeInfoEnumLabelsCount ai) (CommonRaw.attributeInfoEnumLabels ai)
  pure $
    AttributeInfo
      (CommonRaw.attributeInfoWritable ai)
      (CommonRaw.attributeInfoDataFormat ai)
      (CommonRaw.attributeInfoDataType ai)
      (fromIntegral (CommonRaw.attributeInfoMaxDimX ai))
      (fromIntegral (CommonRaw.attributeInfoMaxDimY ai))
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
      (CommonRaw.attributeInfoDispLevel ai)
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
