{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tango.Server
  ( tango_server_set_status,
    tango_server_start,
    tango_server_init,
    tango_server_add_attribute_definition,
    tango_server_set_state,
    createFnWrapper,
    HaskellAttributeDefinition (..),
  )
where

import Data.Int (Int32)
import Data.List (find)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Storable (alignment, peek, poke, sizeOf), peekByteOff, pokeByteOff)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CBool, CChar, CDouble, CFloat, CInt (CInt), CLong (CLong), CShort, CUInt, CULong, CUShort)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Tango.Common (HaskellAttrWriteType, HaskellTangoDataType)

#include <c_tango.h>

foreign import ccall "c_tango.h tango_server_init"
  tango_server_init :: CInt -> Ptr CString -> CString -> CInt -> IO ()

foreign import ccall "c_tango.h tango_server_start"
  tango_server_start :: IO ()

type TangoDevLong64 = CLong

foreign import ccall "wrapper" createFnWrapper :: (Ptr () -> IO ()) -> IO (FunPtr (Ptr () -> IO ()))

data HaskellAttributeDefinition = HaskellAttributeDefinition
  { attribute_name :: !CString,
    data_type :: !HaskellTangoDataType,
    write_type :: HaskellAttrWriteType,
    set_callback :: FunPtr (Ptr () -> IO ()),
    get_callback :: FunPtr (Ptr () -> IO ()),
    finalizer_callback :: FunPtr (Ptr () -> IO ())
  }
  deriving (Show, Generic)

instance GStorable HaskellAttributeDefinition

foreign import ccall "c_tango.h tango_server_add_attribute_definition"
  tango_server_add_attribute_definition :: Ptr HaskellAttributeDefinition -> IO ()

foreign import ccall "c_tango.h tango_server_set_status"
  tango_server_set_status :: CString -> IO ()

foreign import ccall "c_tango.h tango_server_set_state"
  tango_server_set_state :: CInt -> IO ()
