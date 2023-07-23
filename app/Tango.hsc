{-# LANGUAGE CPP                         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tango where

import Foreign(Storable(peek, poke, alignment, sizeOf), pokeByteOff, peekArray, peekByteOff)
import Foreign.C.String(peekCString, CString, castCharToCChar)
import Foreign.C.Types(CULong, CBool, CDouble, CInt, CLong, CUInt, CChar)
import Data.Word(Word64, Word32)
import Data.Int(Int32)
import Foreign.Ptr(Ptr)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import Data.Text(Text, pack)
import qualified Data.Vector.Storable as V

#include <c_tango.h>

-- Taken from https://github.com/ifesdjeen/haskell-ffi-tutorial/blob/d93f5354177ec69fcc937803695d07c3b8121bd3/src/Example.hsc
-- #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

type TangoDataType = CInt

data HaskellTangoAttributeData = HaskellDoubleArray HaskellVarDoubleArray
                               | HaskellBoolArray HaskellVarBoolArray
                               deriving(Show)

newDoubleArray :: [CDouble] -> HaskellTangoAttributeData
newDoubleArray = HaskellDoubleArray . HaskellVarDoubleArray . V.fromList

data HaskellTangoDataType = HaskellDevBoolean | HaskellDevDouble | HaskellUnknown deriving(Show)

dataTypeFromHaskell :: HaskellTangoDataType -> CInt
dataTypeFromHaskell HaskellDevBoolean = 1
dataTypeFromHaskell HaskellDevDouble = 5
dataTypeFromHaskell _ = 10

stringToVector :: String -> V.Vector CChar
stringToVector s = V.fromList (castCharToCChar <$> s)

data HaskellDataFormat = HaskellScalar | HaskellSpectrum | HaskellImage deriving(Show)

data HaskellDataQuality = HaskellValid | HaskellInvalid | HaskellAlarm | HaskellChanging | HaskellWarning  deriving(Show)

data HaskellAttributeData = HaskellAttributeData
  { dataFormat :: HaskellDataFormat
  , dataQuality :: HaskellDataQuality
  , nbRead :: CLong
  , name :: V.Vector CChar
  , dimX :: Int32
  , dimY :: Int32
  , dataType :: HaskellTangoDataType
  , tangoAttributeData :: HaskellTangoAttributeData
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
    let withoutType = HaskellAttributeData
                      (formatToHaskell data_format')
                      (qualityToHaskell quality')
                      nb_read'
                      (V.fromList (castCharToCChar <$> nameAsCString))
                      dim_x'
                      dim_y'
    case data_type' :: CInt of
      5 -> do
        attr_data' :: HaskellVarDoubleArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevDouble (HaskellDoubleArray attr_data'))
      1 -> do
        attr_data' :: HaskellVarBoolArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevBoolean (HaskellBoolArray attr_data'))
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
      HaskellDoubleArray doubles -> do
        hPutStrLn stderr "poking doubles"
        (#poke AttributeData, attr_data) ptr doubles
      _ -> pure ()
      

data HaskellDevFailed = HaskellDevFailed
  { devFailedDesc :: CString,
    devFailedReason :: CString,
    devFailedOrigin :: CString,
    devFailedSeverity :: CInt
  }

data HaskellErrorStack = HaskellErrorStack
  { errorStackLength :: Word32,
    errorStackSequence :: Ptr HaskellDevFailed
  }

instance Storable HaskellDevFailed where
  sizeOf _ = (#size ErrorStack)
  alignment _ = (#alignment ErrorStack)
  peek ptr = do
    desc' <- (#peek DevFailed, desc) ptr
    reason' <- (#peek DevFailed, reason) ptr
    origin' <- (#peek DevFailed, origin) ptr
    severity' <- (#peek DevFailed, severity) ptr
    pure (HaskellDevFailed desc' reason' origin' severity')
  poke ptr (HaskellDevFailed desc' reason' origin' severity') = do
    (#poke DevFailed, desc) ptr desc'
    (#poke DevFailed, reason) ptr reason'
    (#poke DevFailed, origin) ptr origin'

newtype HaskellVarDoubleArray = HaskellVarDoubleArray {
    doubles :: V.Vector CDouble
  } deriving(Show)
  
newtype HaskellVarBoolArray = HaskellVarBoolArray {
    bools :: V.Vector CBool
  } deriving(Show)

instance Storable HaskellVarDoubleArray where
  sizeOf _ = (#size VarDoubleArray)
  alignment _ = (#alignment VarDoubleArray)
  peek ptr = do
    length :: CULong <- (#peek VarDoubleArray, length) ptr
    sequence <- (#peek VarDoubleArray, sequence) ptr
    haskellSequence <- peekArray (fromIntegral length) sequence
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
    length :: CULong <- (#peek VarBoolArray, length) ptr
    sequence <- (#peek VarBoolArray, sequence) ptr
    -- FIXME: we could use a custom peekArray for Vectors to be faster possibly?
    haskellSequence <- peekArray (fromIntegral length) sequence
    pure (HaskellVarBoolArray (V.fromList haskellSequence))
  poke ptr (HaskellVarBoolArray content) = do
    hPutStrLn stderr "poking varboolarray"
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke VarBoolArray, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke VarBoolArray, sequence) ptr vptr
  

foreign import ccall unsafe "c_tango.h tango_create_device_proxy"
     tango_create_device_proxy :: CString -> Ptr (Ptr ()) -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_read_attribute"
     tango_read_attribute :: Ptr () -> CString -> Ptr HaskellAttributeData -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_write_attribute"
     tango_write_attribute :: Ptr () -> Ptr HaskellAttributeData -> IO (Ptr HaskellErrorStack)
