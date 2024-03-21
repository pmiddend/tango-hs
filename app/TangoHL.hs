{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TangoHL
  ( withDeviceProxy,
    checkResult,
    readStringAttribute,
    writeIntAttribute,
    commandInOutVoid,
    newDeviceProxy,
    readIntAttribute,
    tangoUrlFromText,
  )
where

import Control.Applicative (pure)
import Control.Exception (Exception, bracket, throw)
import Control.Monad (fail, void, when, (>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool)
import Data.Char (Char)
import Data.Eq ((/=))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int, Int16, Int32, Int64)
import Data.Semigroup ((<>))
import Data.String (String, unlines)
import Data.Text (Text, pack, unpack)
import Data.Traversable (traverse)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.IO (IO)
import Tango.Common
  ( DeviceProxyPtr,
    HaskellAttributeData (..),
    HaskellAttributeDataList (attributeDataListSequence),
    HaskellCommandData (..),
    HaskellDataFormat (..),
    HaskellDataQuality (..),
    HaskellDevFailed (HaskellDevFailed),
    HaskellErrorStack (errorStackLength, errorStackSequence),
    HaskellTangoAttributeData (HaskellAttributeDataLong64Array, HaskellAttributeDataLongArray, HaskellAttributeDataStringArray),
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
import Text.Show (Show, show)
import qualified UnliftIO as UnliftIO
import qualified UnliftIO.Foreign as UnliftForeign
import Prelude (Double, Float, error, fromIntegral)

newtype TangoException = TangoException [HaskellDevFailed Text] deriving (Show)

instance Exception TangoException

checkResult :: (UnliftIO.MonadUnliftIO m) => m (Ptr HaskellErrorStack) -> m ()
checkResult action = do
  es <- action
  when (es /= nullPtr) $ do
    errorStack <- liftIO $ peek es
    stackItems <- UnliftForeign.peekArray (fromIntegral (errorStackLength errorStack)) (errorStackSequence errorStack)
    formattedStackItems :: [HaskellDevFailed Text] <- traverse (traverse ((pack <$>) . UnliftForeign.peekCString)) stackItems
    throw (TangoException formattedStackItems)

newtype TangoUrl = TangoUrl {getTangoUrl :: Text}

tangoUrlFromText :: Text -> TangoUrl
tangoUrlFromText = TangoUrl

newDeviceProxy :: forall m. (UnliftIO.MonadUnliftIO m) => TangoUrl -> m DeviceProxyPtr
newDeviceProxy (TangoUrl url) =
  liftIO $ alloca $ \proxyPtrPtr -> do
    withCString (unpack url) $ \proxyName -> do
      checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
      peek proxyPtrPtr

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
  UnliftForeign.withCString (unpack attributeName) $ \attributeNameC ->
    UnliftForeign.with (fromIntegral newValue) $ \newValuePtr -> UnliftForeign.with
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

readIntAttribute :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> m Int
readIntAttribute proxyPtr attributeNameHaskell =
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
          error "invalid type of attribute, not a string"

commandInOutVoid :: (UnliftIO.MonadUnliftIO m) => DeviceProxyPtr -> Text -> m ()
commandInOutVoid proxyPtr commandName =
  liftIO $
    withCString (unpack commandName) $
      \commandNamePtr ->
        with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \commandDataInPtr -> with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \commandDataOutPtr ->
          checkResult $ tango_command_inout proxyPtr commandNamePtr commandDataInPtr commandDataOutPtr

-- getTimeoutMillis :: DeviceProxyPtr -> IO Int
-- getTimeoutMillis proxyPtr = alloca $ \millisPtr -> do
--   checkResult (tango_get_timeout_millis proxyPtr millisPtr)
--   fromIntegral <$> peek millisPtr

-- setTimeoutMillis :: DeviceProxyPtr -> Int -> IO ()
-- setTimeoutMillis proxyPtr millis = checkResult (tango_set_timeout_millis proxyPtr (fromIntegral millis))

-- data DevEncoded = DevEncoded
--   { encodedFormat :: Text,
--     encodedData :: [Word8]
--   }
--   deriving (Show)

-- data CommandInputOutput
--   = CommandInputVoid
--   | CommandInputBool Bool
--   | CommandInputInt16 Int16
--   | CommandInputInt32 Int32
--   | CommandInputInt64 Int64
--   | CommandInputFloat Float
--   | CommandInputDouble Double
--   | CommandInputWord8 Word8
--   | CommandInputWord16 Word16
--   | CommandInputWord32 Word32
--   | CommandInputWord64 Word64
--   | CommandInputText Text
--   | CommandInputBoolList [Bool]
--   | CommandInputCharList [Char]
--   | CommandInputInt16List [Int16]
--   | CommandInputInt32List [Int32]
--   | CommandInputFloatList [Float]
--   | CommandInputDoubleList [Double]
--   | CommandInputWord16List [Word16]
--   | CommandInputWord32List [Word32]
--   | CommandInputTextList [Text]
--   | CommandInputInt32TextList [Int32] [Text]
--   | CommandInputDoubleTextList [Double] [Text]
--   | CommandInputState HaskellTangoDevState
--   | CommandInputConstDevText Text
--   | CommandInputDevEncoded DevEncoded
--   deriving (Show)

-- haskellCommandDataToCommandInputOutput :: HaskellTangoCommandData -> IO CommandInputOutput
-- haskellCommandDataToCommandInputOutput x = case x of
--   HaskellCommandVoid -> pure CommandInputVoid
--   HaskellCommandBool b -> pure (CommandInputBool (b /= 0))
--   HaskellCommandVarCString (HaskellTangoVarArray len values) -> do
--     peekedCStrings :: [CString] <- peekArray (fromIntegral len) values
--     peekedTexts :: [Text] <- traverse ((pack <$>) . peekCString) peekedCStrings
--     pure (CommandInputTextList peekedTexts)

-- commandInout :: DeviceProxyPtr -> Text -> CommandInputOutput -> (CommandInputOutput -> IO a) -> IO a
-- commandInout proxyPtr commandName input outputProcessor =
--   withCString (unpack commandName) $ \commandNamePtr ->
--     case input of
--       CommandInputVoid -> with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \inputPtr -> alloca $ \outputPtr ->
--         let initialize = do
--               checkResult (tango_command_inout proxyPtr commandNamePtr inputPtr outputPtr)
--               peek outputPtr
--             deinitialize _ = tango_free_CommandData outputPtr
--          in bracket initialize deinitialize (\x -> haskellCommandDataToCommandInputOutput (tangoCommandData x) >>= outputProcessor)
--       CommandInputBool b -> with (HaskellCommandData HaskellDevBoolean (HaskellCommandBool (if b then 1 else 0))) $ \inputPtr -> alloca $ \outputPtr ->
--         let initialize = do
--               checkResult (tango_command_inout proxyPtr commandNamePtr inputPtr outputPtr)
--               peek outputPtr
--             deinitialize _ = tango_free_CommandData outputPtr
--          in bracket initialize deinitialize (\x -> haskellCommandDataToCommandInputOutput (tangoCommandData x) >>= outputProcessor)
--       CommandInputTextList b -> -- FIXME
