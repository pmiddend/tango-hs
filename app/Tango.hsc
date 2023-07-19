module Tango where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#include <c_tango.h>

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
