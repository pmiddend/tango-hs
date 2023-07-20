module Tango where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#include <c_tango.h>

type TangoDataType = CInt

data TangoAttributeData = TangoAttributeDataStringArray [CString]
                        | TangoAttributeDataLongArray [CLong]

data AttributeData = AttributeData
  { attrData :: TangoAttributeData,
    dataFormat :: AttrDataFormat,
    quality :: AttrQuality,
    nbRead :: CLong,
    name :: CString,
    dimX :: CInt,
    dimY :: CInt,
    timeStamp :: timeval
  }

instance Storable AttributeData where
  sizeOf _ = (#size AttributeData)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    data_type' <- (#peek AttributeData, data_type) ptr
    attr_data' <- (#peek AttributeData, attr_data) ptr
    data_format' <- (#peek AttributeData, data_format) ptr
    quality' <- (#peek AttributeData, quality) ptr
    nb_read' <- (#peek AttributeData, nb_read) ptr
    name' <- (#peek AttributeData, name) ptr
    dim_x' <- (#peek AttributeData, dim_x) ptr
    dim_y' <- (#peek AttributeData, dim_y) ptr
    time_stamp' <- (#peek AttributeData, time_stamp) ptr
    case fromIntegral data_type' of
      3 -> do
        (#peek TangoAttributeData, long_arr) data_type'
        pure (AttributeData (attr_data') data_format' quality' nb_read' name' dim_x' dim_y' time_stamp')
  poke ptr (AttributeData data_type' attr_data' data_format' quality' nb_read' name' dim_x' dim_y' time_stamp') = do
    (#poke AttributeData, data_type) ptr data_type'
    (#poke AttributeData, attr_data) ptr attr_data'
    (#poke AttributeData, data_format) ptr data_format'
    (#poke AttributeData, quality) ptr quality'
    (#poke AttributeData, nb_read) ptr nb_read'
    (#poke AttributeData, name) ptr name'
    (#poke AttributeData, dim_x) ptr dim_x'
    (#poke AttributeData, dim_y) ptr dim_y'
    (#poke AttributeData, time_stamp) ptr time_stamp'

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
  alignment _ = alignment (undefined :: CInt)
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
    (#poke DevFailed, severity) ptr severity'
  

foreign import ccall unsafe "c_tango.h tango_create_device_proxy"
     tango_create_device_proxy :: CString -> Ptr (Ptr ()) -> IO (Ptr HaskellErrorStack)

foreign import ccall unsafe "c_tango.h tango_read_attribute"
     tango_read_attribute :: Ptr () -> CString -> Ptr AttributeData -> IO (Ptr HaskellErrorStack)
