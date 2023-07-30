{-# LANGUAGE CPP                         #-}
{-# LANGUAGE DeriveFunctor                         #-}
{-# LANGUAGE DeriveFoldable                         #-}
{-# LANGUAGE DeriveTraversable                         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tango(tango_create_device_proxy,
             tango_delete_device_proxy,
             tango_read_attribute,
             tango_write_attribute,
             tango_command_inout,
             tango_free_AttributeData,
             tango_free_CommandData,
             tango_set_timeout_millis,
             tango_get_timeout_millis,
             tango_set_source,
             tango_get_source,
             haskellDevSourceDev,
             haskellDevSourceCache,
             haskellDevSourceCacheDev,
             HaskellErrorStack(..),
             HaskellDevFailed(..),
             HaskellAttributeData(..),
             HaskellCommandData(..),
             HaskellTangoDataType(HaskellDevBoolean, HaskellDevDouble, HaskellDevString),
             HaskellTangoCommandData(HaskellCommandDouble, HaskellCommandString),
             HaskellTangoAttributeData(..),
             newDoubleArray,
             stringToVector
             ) where

import Foreign(Storable(peek, poke, alignment, sizeOf), pokeByteOff, peekArray, peekByteOff)
import Foreign.C.String(peekCString, CString, castCharToCChar)
import Foreign.C.Types(CULong, CBool, CDouble, CInt, CLong, CChar, CInt(CInt))
import Data.Word(Word32)
import Data.Int(Int32)
import Foreign.Ptr(Ptr)
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector.Storable as V

#include <c_tango.h>

-- Taken from https://github.com/ifesdjeen/haskell-ffi-tutorial/blob/d93f5354177ec69fcc937803695d07c3b8121bd3/src/Example.hsc
-- #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

haskellDevSourceDev :: CInt
haskellDevSourceDev = 0

haskellDevSourceCache :: CInt
haskellDevSourceCache = 1

haskellDevSourceCacheDev :: CInt
haskellDevSourceCacheDev = 2
  
data HaskellTangoCommandData = HaskellCommandDouble CDouble
                             | HaskellCommandString (V.Vector CChar)
                               deriving(Show)

data HaskellTangoAttributeData = HaskellDoubleArray HaskellVarDoubleArray
                               | HaskellBoolArray HaskellVarBoolArray
                               | HaskellStringArray HaskellVarStringArray
                               deriving(Show)

newDoubleArray :: [CDouble] -> HaskellTangoAttributeData
newDoubleArray = HaskellDoubleArray . HaskellVarDoubleArray . V.fromList

-- withStringArray :: [String] -> (V.Vector CString -> IO b) -> IO b
-- withStringArray strings =
--   let retrieveStrings = V.fromList <$> traverse newCString strings
--       destroyStrings = traverse free . V.toList
--   in bracket retrieveStrings destroyStrings

data HaskellTangoDataType = HaskellDevBoolean
                          | HaskellDevDouble
                          | HaskellDevString
                          | HaskellUnknown
                          deriving(Show)

dataTypeFromHaskell :: HaskellTangoDataType -> CInt
dataTypeFromHaskell HaskellDevBoolean = 1
dataTypeFromHaskell HaskellDevDouble = 5
dataTypeFromHaskell HaskellDevString = 8
dataTypeFromHaskell _ = 10

stringToVector :: String -> V.Vector CChar
stringToVector s = V.fromList (castCharToCChar <$> s)

data HaskellDataFormat = HaskellScalar | HaskellSpectrum | HaskellImage deriving(Show)

data HaskellDataQuality = HaskellValid | HaskellInvalid | HaskellAlarm | HaskellChanging | HaskellWarning  deriving(Show)

data Timeval = Timeval {
  -- Guesswork, not sure how to type it
    tvSec :: CLong
  , tvUsec :: CLong
  } deriving(Show)

instance Storable Timeval where
  sizeOf _ = (#size timeval)
  alignment _ = (#alignment timeval)
  peek ptr = do
    tvSec' <- (#peek timeval, tv_sec) ptr
    tvUsec' <- (#peek timeval, tv_usec) ptr
    pure (Timeval tvSec' tvUsec')
  poke ptr (Timeval tvSec' tvUsec') = do
    (#poke timeval, tv_sec) ptr tvSec'
    (#poke timeval, tv_usec) ptr tvUsec'

data HaskellAttributeData = HaskellAttributeData
  { dataFormat :: HaskellDataFormat
  , dataQuality :: HaskellDataQuality
  , nbRead :: CLong
  , name :: V.Vector CChar
  , dimX :: Int32
  , dimY :: Int32
  , timeStamp :: Timeval
  , dataType :: HaskellTangoDataType
  , tangoAttributeData :: HaskellTangoAttributeData
  } deriving(Show)

data HaskellCommandData = HaskellCommandData
  { argType :: HaskellTangoDataType
  , tangoCommandData :: HaskellTangoCommandData
  } deriving(Show)

qualityToHaskell :: CInt -> HaskellDataQuality
qualityToHaskell 0 = HaskellValid
qualityToHaskell 1 = HaskellInvalid
qualityToHaskell 2 = HaskellAlarm
qualityToHaskell 3 = HaskellChanging
qualityToHaskell _ = HaskellWarning

formatToHaskell :: CInt -> HaskellDataFormat
formatToHaskell 0 = HaskellScalar
formatToHaskell 1 = HaskellSpectrum
formatToHaskell _ = HaskellImage

instance Storable HaskellAttributeData where
  sizeOf _ = (#{size AttributeData})
  alignment _ = (#alignment AttributeData)
  peek ptr = do
    data_type' <- (#peek AttributeData, data_type) ptr
    dim_x' <- (#peek AttributeData, dim_x) ptr
    dim_y' <- (#peek AttributeData, dim_y) ptr
    name' <- (#peek AttributeData, name) ptr
    nameAsCString <- peekCString name'
    nb_read' <- (#peek AttributeData, nb_read) ptr
    quality' <- (#peek AttributeData, quality) ptr
    data_format' <- (#peek AttributeData, data_format) ptr
    time_stamp' <- (#peek AttributeData, time_stamp) ptr
    let withoutType = HaskellAttributeData
                      (formatToHaskell data_format')
                      (qualityToHaskell quality')
                      nb_read'
                      (V.fromList (castCharToCChar <$> nameAsCString))
                      dim_x'
                      dim_y'
                      time_stamp'
    case data_type' :: CInt of
      5 -> do
        attr_data' :: HaskellVarDoubleArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevDouble (HaskellDoubleArray attr_data'))
      1 -> do
        attr_data' :: HaskellVarBoolArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevBoolean (HaskellBoolArray attr_data'))
      8 -> do
        attr_data' :: HaskellVarStringArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevString (HaskellStringArray attr_data'))
      _ -> error "shit"
  poke ptr haskellAttributeData = do
    hPutStrLn stderr "poking x"
    (#poke AttributeData, dim_x) ptr (dimX haskellAttributeData)
    hPutStrLn stderr "poking y"
    (#poke AttributeData, dim_y) ptr (dimY haskellAttributeData)
    hPutStrLn stderr "poking name"
    V.unsafeWith (name haskellAttributeData) $ \namePtr -> (#poke AttributeData, name) ptr namePtr
    (#poke AttributeData, data_type) ptr (dataTypeFromHaskell (dataType haskellAttributeData))
    case tangoAttributeData haskellAttributeData of
      HaskellDoubleArray doubles' -> do
        hPutStrLn stderr "poking doubles"
        (#poke AttributeData, attr_data) ptr doubles'
      HaskellStringArray strings' -> do
        hPutStrLn stderr "poking strings"
        (#poke AttributeData, attr_data) ptr strings'
      _ -> pure ()
      
instance Storable HaskellCommandData where
  sizeOf _ = (#{size CommandData})
  alignment _ = (#alignment CommandData)
  peek ptr = do
    data_type' <- (#peek CommandData, arg_type) ptr
    case data_type' :: CInt of
      5 -> do
        cmd_data' :: CDouble <- (#peek CommandData, cmd_data) ptr
        pure (HaskellCommandData HaskellDevDouble (HaskellCommandDouble cmd_data'))
      8 -> do
        cmd_data' :: CString <- (#peek CommandData, cmd_data) ptr
        real_string <- peekCString cmd_data'
        pure (HaskellCommandData HaskellDevString (HaskellCommandString (V.fromList (castCharToCChar <$> real_string))))
      _ -> error "shit"
  poke ptr haskellCommandData = do
    (#poke CommandData, arg_type) ptr (dataTypeFromHaskell (argType haskellCommandData))
    case tangoCommandData haskellCommandData of
      HaskellCommandDouble double -> do
        hPutStrLn stderr "poking double"
        (#poke CommandData, cmd_data) ptr double
      HaskellCommandString charVector -> do
        hPutStrLn stderr "poking string"
        V.unsafeWith charVector ((#poke CommandData, cmd_data) ptr)

data HaskellDevFailed a = HaskellDevFailed
  { devFailedDesc :: a,
    devFailedReason :: a,
    devFailedOrigin :: a,
    devFailedSeverity :: CInt
  } deriving(Functor, Foldable, Traversable)

data HaskellErrorStack = HaskellErrorStack
  { errorStackLength :: Word32,
    errorStackSequence :: Ptr (HaskellDevFailed CString)
  }

instance Storable a => Storable (HaskellDevFailed a) where
  sizeOf _ = (#size ErrorStack)
  alignment _ = (#alignment ErrorStack)
  peek ptr = do
    desc' <- (#peek DevFailed, desc) ptr
    reason' <- (#peek DevFailed, reason) ptr
    origin' <- (#peek DevFailed, origin) ptr
    severity' <- (#peek DevFailed, severity) ptr
    pure (HaskellDevFailed desc' reason' origin' severity')
  poke ptr (HaskellDevFailed desc' reason' origin' _severity) = do
    (#poke DevFailed, desc) ptr desc'
    (#poke DevFailed, reason) ptr reason'
    (#poke DevFailed, origin) ptr origin'

newtype HaskellVarDoubleArray = HaskellVarDoubleArray {
    doubles :: V.Vector CDouble
  } deriving(Show)
  
newtype HaskellVarBoolArray = HaskellVarBoolArray {
    bools :: V.Vector CBool
  } deriving(Show)

newtype HaskellVarStringArray = HaskellVarStringArray {
    strings :: V.Vector CString
  } deriving(Show)

instance Storable HaskellVarDoubleArray where
  sizeOf _ = (#size VarDoubleArray)
  alignment _ = (#alignment VarDoubleArray)
  peek ptr = do
    length' :: CULong <- (#peek VarDoubleArray, length) ptr
    sequence' <- (#peek VarDoubleArray, sequence) ptr
    haskellSequence <- peekArray (fromIntegral length') sequence'
    pure (HaskellVarDoubleArray (V.fromList haskellSequence))
  poke ptr (HaskellVarDoubleArray content) = do
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke VarDoubleArray, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke VarBoolArray, sequence) ptr vptr
    

instance Storable HaskellVarBoolArray where
  sizeOf _ = (#size VarBoolArray)
  alignment _ = (#alignment VarBoolArray)
  peek ptr = do
    length' :: CULong <- (#peek VarBoolArray, length) ptr
    sequence' <- (#peek VarBoolArray, sequence) ptr
    -- FIXME: we could use a custom peekArray for Vectors to be faster possibly?
    haskellSequence <- peekArray (fromIntegral length') sequence'
    pure (HaskellVarBoolArray (V.fromList haskellSequence))
  poke ptr (HaskellVarBoolArray content) = do
    hPutStrLn stderr "poking varboolarray"
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke VarBoolArray, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke VarBoolArray, sequence) ptr vptr

instance Storable HaskellVarStringArray where
  sizeOf _ = (#size VarStringArray)
  alignment _ = (#alignment VarStringArray)
  peek ptr = do
    length' :: CULong <- (#peek VarStringArray, length) ptr
    sequence' <- (#peek VarStringArray, sequence) ptr
    -- FIXME: we could use a custom peekArray for Vectors to be faster possibly?
    haskellSequence <- peekArray (fromIntegral length') sequence'
    pure (HaskellVarStringArray (V.fromList haskellSequence))
  poke ptr (HaskellVarStringArray content) = do
    hPutStrLn stderr "poking varstringarray"
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke VarStringArray, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke VarStringArray, sequence) ptr vptr
  

foreign import ccall unsafe "c_tango.h tango_create_device_proxy"
     tango_create_device_proxy :: CString -> Ptr (Ptr ()) -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_delete_device_proxy"
     tango_delete_device_proxy :: Ptr () -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_read_attribute"
     tango_read_attribute :: Ptr () -> CString -> Ptr HaskellAttributeData -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_write_attribute"
     tango_write_attribute :: Ptr () -> Ptr HaskellAttributeData -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_command_inout"
     tango_command_inout :: Ptr () -> CString -> Ptr HaskellCommandData -> Ptr HaskellCommandData -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_free_AttributeData"
     tango_free_AttributeData :: Ptr HaskellAttributeData -> IO ()

foreign import ccall unsafe "c_tango.h tango_free_CommandData"
     tango_free_CommandData :: Ptr HaskellCommandData -> IO ()

foreign import ccall unsafe "c_tango.h tango_set_timeout_millis"
     tango_set_timeout_millis :: Ptr () -> CInt -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_get_timeout_millis"
     tango_get_timeout_millis :: Ptr () -> Ptr CInt -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_set_source"
     tango_set_source :: Ptr () -> CInt -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_get_source"
     tango_get_source :: Ptr () -> Ptr CInt -> IO (Ptr HaskellErrorStack)

