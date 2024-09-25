{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tango.Client
  ( withDeviceProxy,
    checkResult,
    readStringAttribute,
    writeIntAttribute,
    writeDoubleAttribute,
    commandInOutVoid,
    HaskellDevFailed (HaskellDevFailed),
    devFailedDesc,
    throwTangoException,
    devFailedReason,
    devFailedOrigin,
    devFailedSeverity,
    newDeviceProxy,
    readLong64Attribute,
    readUShortAttribute,
    readULong64Attribute,
    readDoubleAttribute,
    readBoolAttribute,
    readStateAttribute,
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
import System.IO (IO)
import Tango.Raw.Common
  ( DeviceProxyPtr,
    HaskellAttrWriteType (Read, ReadWrite),
    HaskellAttributeData (..),
    HaskellAttributeDataList (attributeDataListSequence),
    HaskellCommandData (..),
    HaskellDataFormat (..),
    HaskellDataQuality (..),
    HaskellDevFailed (HaskellDevFailed, devFailedDesc, devFailedOrigin, devFailedReason, devFailedSeverity),
    HaskellErrorStack (errorStackLength, errorStackSequence),
    HaskellTangoAttributeData (HaskellAttributeDataBoolArray, HaskellAttributeDataDoubleArray, HaskellAttributeDataLong64Array, HaskellAttributeDataLongArray, HaskellAttributeDataStateArray, HaskellAttributeDataStringArray, HaskellAttributeDataULong64Array, HaskellAttributeDataUShortArray),
    HaskellTangoCommandData (..),
    HaskellTangoDataType (..),
    HaskellTangoDevState (..),
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
    tango_throw_exception,
    tango_write_attribute,
  )
import Text.Show (Show, show)
import qualified UnliftIO
import UnliftIO.Environment (getArgs, getProgName)
import UnliftIO.Foreign (CDouble, CLong, FunPtr, alloca, castPtr, newCString, peek, peekArray, peekCString, poke, with, withArray, withCString)
import Prelude (Double, Enum (fromEnum), Float, error, fromIntegral, realToFrac, undefined)

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

-- | Read a string attribute and decode it into a text
readStringAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Text
readStringAttribute proxyPtr (AttributeName attributeNameHaskell) =
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

-- | Read a State attribute
readStateAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> m HaskellTangoDevState
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

readAttributeGeneral :: (MonadIO m) => (HaskellTangoAttributeData -> IO (Maybe a)) -> DeviceProxy -> AttributeName -> m a
readAttributeGeneral extractValue proxyPtr (AttributeName attributeNameHaskell) =
  liftIO $ withCString (unpack attributeNameHaskell) $ \attributeName -> do
    alloca $ \haskellAttributeDataPtr -> do
      checkResult (tango_read_attribute proxyPtr attributeName haskellAttributeDataPtr)
      haskellAttributeData <- peek haskellAttributeDataPtr
      extractedValue <- extractValue (tangoAttributeData haskellAttributeData)
      case extractedValue of
        Nothing -> error ("invalid type for attribute \"" <> unpack attributeNameHaskell <> "\"")
        Just v ->
          pure v

-- | Read an ULong64 attribute
readULong64Attribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Word64
readULong64Attribute = readAttributeGeneral extract
  where
    extract (HaskellAttributeDataULong64Array (HaskellTangoVarArray {varArrayValues})) = Just . fromIntegral <$> peek varArrayValues
    extract _ = pure Nothing

readLong64Attribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Int64
readLong64Attribute =
  readAttributeGeneral extract
  where
    extract (HaskellAttributeDataLong64Array (HaskellTangoVarArray {varArrayValues})) = Just . fromIntegral <$> peek varArrayValues
    extract _ = pure Nothing

readUShortAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Word16
readUShortAttribute =
  readAttributeGeneral extract
  where
    extract (HaskellAttributeDataUShortArray (HaskellTangoVarArray {varArrayValues})) = Just . fromIntegral <$> peek varArrayValues
    extract _ = pure Nothing

readDoubleAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Double
readDoubleAttribute =
  readAttributeGeneral extract
  where
    extract (HaskellAttributeDataDoubleArray (HaskellTangoVarArray {varArrayValues})) = Just . realToFrac <$> peek varArrayValues
    extract _ = pure Nothing

readBoolAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxy -> AttributeName -> m Bool
readBoolAttribute =
  readAttributeGeneral extract
  where
    extract (HaskellAttributeDataBoolArray (HaskellTangoVarArray {varArrayValues})) = Just . (/= 0) <$> peek varArrayValues
    extract _ = pure Nothing

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
