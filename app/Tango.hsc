{-# LANGUAGE CPP                         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tango where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Data.Word(Word64)
import Foreign.Ptr
import System.IO (hFlush, stdout, hPutStrLn, stderr)

#include <c_tango.h>

-- Taken from https://github.com/ifesdjeen/haskell-ffi-tutorial/blob/d93f5354177ec69fcc937803695d07c3b8121bd3/src/Example.hsc
-- #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

type TangoDataType = CInt

data HaskellTangoAttributeData = HaskellDoubleArray [CDouble]
                               | HaskellBoolArray [CBool]
                               deriving(Show)

data HaskellTangoDataType = HaskellDevBoolean | HaskellDevDouble | HaskellUnknown deriving(Show)

data HaskellDataFormat = HaskellScalar | HaskellSpectrum | HaskellImage deriving(Show)

data HaskellDataQuality = HaskellValid | HaskellInvalid | HaskellAlarm | HaskellChanging | HaskellWarning  deriving(Show)

data HaskellAttributeData = HaskellAttributeData
  { dataFormat :: HaskellDataFormat
  , dataQuality :: HaskellDataQuality
  , nbRead :: CLong
  , name :: CString
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
    nb_read' <- (#peek AttributeData, nb_read) ptr
    quality' <- (#peek AttributeData, quality) ptr
    data_format' <- (#peek AttributeData, data_format) ptr
    let withoutType = HaskellAttributeData (formatToHaskell data_format') (qualityToHaskell quality') nb_read' name' dim_x' dim_y'
    case data_type' :: CInt of
      5 -> do
        attr_data' :: HaskellVarDoubleArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevDouble (HaskellDoubleArray (doubles attr_data')))
      1 -> do
        attr_data' :: HaskellVarBoolArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevBoolean (HaskellBoolArray (bools attr_data')))
      _ -> error "shit"
  poke ptr _ = do
    hPutStrLn stderr "poking"
    pure ()

data HaskellDevFailed = HaskellDevFailed
  { devFailedDesc :: CString,
    devFailedReason :: CString,
    devFailedOrigin :: CString,
    devFailedSeverity :: CInt
  }

data HaskellErrorStack = HaskellErrorStack
  { errorStackLength :: CUInt,
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
    doubles :: [CDouble]
  }
  
newtype HaskellVarBoolArray = HaskellVarBoolArray {
    bools :: [CBool]
  }

instance Storable HaskellVarDoubleArray where
  sizeOf _ = (#size VarDoubleArray)
  alignment _ = (#alignment VarDoubleArray)
  peek ptr = do
    length :: CULong <- (#peek VarDoubleArray, length) ptr
    sequence <- (#peek VarDoubleArray, sequence) ptr
    haskellSequence <- peekArray (fromIntegral length) sequence
    pure (HaskellVarDoubleArray haskellSequence)
  poke ptr _ = do
    hPutStrLn stderr "poking vardoublearray"

instance Storable HaskellVarBoolArray where
  sizeOf _ = (#size VarBoolArray)
  alignment _ = (#alignment VarBoolArray)
  peek ptr = do
    length :: CULong <- (#peek VarBoolArray, length) ptr
    sequence <- (#peek VarBoolArray, sequence) ptr
    haskellSequence <- peekArray (fromIntegral length) sequence
    pure (HaskellVarBoolArray haskellSequence)
  poke ptr _ = do
    hPutStrLn stderr "poking varboolarray"
  

foreign import ccall unsafe "c_tango.h tango_create_device_proxy"
     tango_create_device_proxy :: CString -> Ptr (Ptr ()) -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_read_attribute"
     tango_read_attribute :: Ptr () -> CString -> Ptr HaskellAttributeData -> IO (Ptr HaskellErrorStack)
