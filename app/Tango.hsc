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

-- data TangoAttributeData = TangoAttributeDataStringArray [CString]
--                         | TangoAttributeDataLongArray [CLong]

data HaskellAttributeData = HaskellAttributeData
  { doubles :: [CDouble]
    -- dataFormat :: AttrDataFormat,
    -- quality :: AttrQuality,
    -- nbRead :: CLong,
    -- name :: CString,
    -- dimX :: CInt,
    -- dimY :: CInt
    -- timeStamp :: timeval
  }

instance Storable HaskellAttributeData where
  sizeOf _ = (#{size AttributeData})
  alignment _ = (#alignment AttributeData)
  peek ptr = do
    data_type' <- (#peek AttributeData, data_type) ptr
    -- attr_data' <- (#peek AttributeData, attr_data) ptr
    -- data_format' <- (#peek AttributeData, data_format) ptr
    -- quality' <- (#peek AttributeData, quality) ptr
    -- nb_read' <- (#peek AttributeData, nb_read) ptr
    -- name' <- (#peek AttributeData, name) ptr
    -- dim_x' <- (#peek AttributeData, dim_x) ptr
    -- dim_y' <- (#peek AttributeData, dim_y) ptr
    -- time_stamp' <- (#peek AttributeData, time_stamp) ptr
    case data_type' :: CInt of
      5 -> do
        hPutStrLn stderr "data type matches"
        attr_data' :: HaskellVarDoubleArray <- (#peek AttributeData, attr_data) ptr
        -- doubleArray <- (#peek TangoAttributeData, double_arr) attr_data'
        -- doubleSequenceLen :: CSize <- (#peek VarDoubleArray, length) attr_data'
        -- doubleSequenceLen :: CSize <- (#peek VarDoubleArray, length) attr_data'
        -- hPutStrLn stderr ("double len: " <> show doubleSequenceLen)
        -- doubleSequence :: Ptr CDouble <- (#peek VarDoubleArray, sequence) doubleArray
        -- doubleSequence :: Ptr CDouble <- (#peek VarDoubleArray, sequence) attr_data'
        -- doubleList :: [CDouble] <- peekArray (fromIntegral doubleSequenceLen) doubleSequence
        -- doubleList :: CDouble <- peek doubleSequence
        -- pure (HaskellAttributeData [realToFrac doubleList])
        -- doubleArray :: HaskellVarDoubleArray <- peek attr_data'
        -- pure (HaskellAttributeData (realToFrac <$> content doubleArray))
        -- doubleArray <- (#peek TangoAttributeData, double_arr) attr_data'
        -- length :: Word64 <- (#peek VarDoubleArray, length) doubleArray
        -- length :: Word32 <- peek attr_data'
        hPutStrLn stderr ("len: " <> show (content attr_data'))
        pure (HaskellAttributeData [])
      _ -> pure (HaskellAttributeData [])
  poke ptr (HaskellAttributeData doubles) = do
    hPutStrLn stderr "poking"
    pure ()
    -- (#poke AttributeData, data_type) ptr 5
     -- (#poke AttributeData, data_type) ptr data_type'
     -- (#poke AttributeData, attr_data) ptr attr_data'
    
    -- (#poke AttributeData, data_type) ptr data_type'
  -- poke ptr (AttributeData data_type' attr_data' data_format' quality' nb_read' name' dim_x' dim_y' time_stamp') = do
  --   (#poke AttributeData, data_type) ptr data_type'
  --   (#poke AttributeData, attr_data) ptr attr_data'
  --   (#poke AttributeData, data_format) ptr data_format'
  --   (#poke AttributeData, quality) ptr quality'
  --   (#poke AttributeData, nb_read) ptr nb_read'
  --   (#poke AttributeData, name) ptr name'
  --   (#poke AttributeData, dim_x) ptr dim_x'
  --   (#poke AttributeData, dim_y) ptr dim_y'
  --   (#poke AttributeData, time_stamp) ptr time_stamp'

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

data HaskellVarDoubleArray = HaskellVarDoubleArray {
    content :: [CDouble]
  }

instance Storable HaskellVarDoubleArray where
  sizeOf _ = (#size VarDoubleArray)
  alignment _ = (#alignment VarDoubleArray)
  peek ptr = do
    length :: CULong <- (#peek VarDoubleArray, length) ptr
    hPutStrLn stderr ("length got " <> show length)
    sequence <- (#peek VarDoubleArray, sequence) ptr
    haskellSequence <- peekArray (fromIntegral length) sequence
    pure (HaskellVarDoubleArray haskellSequence)
  poke ptr _ = do
    hPutStrLn stderr "poking vardoublearray"
  

foreign import ccall unsafe "c_tango.h tango_create_device_proxy"
     tango_create_device_proxy :: CString -> Ptr (Ptr ()) -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_read_attribute"
     tango_read_attribute :: Ptr () -> CString -> Ptr HaskellAttributeData -> IO (Ptr HaskellErrorStack)
