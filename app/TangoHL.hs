{-# LANGUAGE ScopedTypeVariables #-}

module TangoHL (withDeviceProxy, checkResult, getTimeoutMillis, setTimeoutMillis) where

import Control.Applicative (pure)
import Control.Exception (Exception, bracket, throw)
import Control.Monad (fail, when, (>>=))
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
import Tango
  ( DeviceProxyPtr,
    HaskellCommandData (..),
    HaskellDevFailed (HaskellDevFailed),
    HaskellErrorStack (errorStackLength, errorStackSequence),
    HaskellTangoCommandData (..),
    HaskellTangoDataType (..),
    HaskellTangoDevState,
    HaskellTangoVarArray (..),
    tango_command_inout,
    tango_create_device_proxy,
    tango_delete_device_proxy,
    tango_free_CommandData,
    tango_get_timeout_millis,
    tango_set_timeout_millis,
  )
import Text.Show (Show, show)
import Prelude (Double, Float, fromIntegral)

newtype TangoException = TangoException [HaskellDevFailed Text] deriving (Show)

instance Exception TangoException

checkResult :: IO (Ptr HaskellErrorStack) -> IO ()
checkResult action = do
  es <- action
  when (es /= nullPtr) $ do
    errorStack <- peek es
    stackItems <- peekArray (fromIntegral (errorStackLength errorStack)) (errorStackSequence errorStack)
    formattedStackItems :: [HaskellDevFailed Text] <- traverse (traverse ((pack <$>) . peekCString)) stackItems
    throw (TangoException formattedStackItems)

withDeviceProxy :: Text -> (DeviceProxyPtr -> IO a) -> IO a
withDeviceProxy proxyAddress =
  let initialize :: IO DeviceProxyPtr
      initialize =
        alloca $ \proxyPtrPtr -> do
          withCString (unpack proxyAddress) $ \proxyName -> do
            checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
            peek proxyPtrPtr
      deinitialize :: DeviceProxyPtr -> IO ()
      deinitialize proxyPtrPtr =
        checkResult (tango_delete_device_proxy proxyPtrPtr)
   in bracket initialize deinitialize

getTimeoutMillis :: DeviceProxyPtr -> IO Int
getTimeoutMillis proxyPtr = alloca $ \millisPtr -> do
  checkResult (tango_get_timeout_millis proxyPtr millisPtr)
  fromIntegral <$> peek millisPtr

setTimeoutMillis :: DeviceProxyPtr -> Int -> IO ()
setTimeoutMillis proxyPtr millis = checkResult (tango_set_timeout_millis proxyPtr (fromIntegral millis))

data DevEncoded = DevEncoded
  { encodedFormat :: Text,
    encodedData :: [Word8]
  }
  deriving (Show)

data CommandInputOutput
  = CommandInputVoid
  | CommandInputBool Bool
  | CommandInputInt16 Int16
  | CommandInputInt32 Int32
  | CommandInputInt64 Int64
  | CommandInputFloat Float
  | CommandInputDouble Double
  | CommandInputWord8 Word8
  | CommandInputWord16 Word16
  | CommandInputWord32 Word32
  | CommandInputWord64 Word64
  | CommandInputText Text
  | CommandInputBoolList [Bool]
  | CommandInputCharList [Char]
  | CommandInputInt16List [Int16]
  | CommandInputInt32List [Int32]
  | CommandInputFloatList [Float]
  | CommandInputDoubleList [Double]
  | CommandInputWord16List [Word16]
  | CommandInputWord32List [Word32]
  | CommandInputTextList [Text]
  | CommandInputInt32TextList [Int32] [Text]
  | CommandInputDoubleTextList [Double] [Text]
  | CommandInputState HaskellTangoDevState
  | CommandInputConstDevText Text
  | CommandInputDevEncoded DevEncoded
  deriving (Show)

haskellCommandDataToCommandInputOutput :: HaskellTangoCommandData -> IO CommandInputOutput
haskellCommandDataToCommandInputOutput x = case x of
  HaskellCommandVoid -> pure CommandInputVoid
  HaskellCommandBool b -> pure (CommandInputBool (b /= 0))
  HaskellCommandVarCString (HaskellTangoVarArray len values) -> do
    peekedCStrings :: [CString] <- peekArray (fromIntegral len) values
    peekedTexts :: [Text] <- traverse ((pack <$>) . peekCString) peekedCStrings
    pure (CommandInputTextList peekedTexts)

commandInout :: DeviceProxyPtr -> Text -> CommandInputOutput -> (CommandInputOutput -> IO a) -> IO a
commandInout proxyPtr commandName input outputProcessor =
  withCString (unpack commandName) $ \commandNamePtr ->
    case input of
      CommandInputVoid -> with (HaskellCommandData HaskellDevVoid HaskellCommandVoid) $ \inputPtr -> alloca $ \outputPtr ->
        let initialize = do
              checkResult (tango_command_inout proxyPtr commandNamePtr inputPtr outputPtr)
              peek outputPtr
            deinitialize _ = tango_free_CommandData outputPtr
         in bracket initialize deinitialize (\x -> haskellCommandDataToCommandInputOutput (tangoCommandData x) >>= outputProcessor)
      CommandInputBool b -> with (HaskellCommandData HaskellDevBoolean (HaskellCommandBool (if b then 1 else 0))) $ \inputPtr -> alloca $ \outputPtr ->
        let initialize = do
              checkResult (tango_command_inout proxyPtr commandNamePtr inputPtr outputPtr)
              peek outputPtr
            deinitialize _ = tango_free_CommandData outputPtr
         in bracket initialize deinitialize (\x -> haskellCommandDataToCommandInputOutput (tangoCommandData x) >>= outputProcessor)
      CommandInputTextList b -> -- FIXME
