{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
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
    tango_server_add_command_definition,
    createCommandCallback,
    createGlobalFinalizer,
    tango_server_add_property,
    tango_server_read_property,
    createDeviceInitCallback,
    DeviceInitCallback,
    DeviceInstancePtr,
    tango_server_set_state,
    createFnWrapper,
    HaskellAttributeDefinition (..),
    HaskellCommandDefinition (..),
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

type GlobalFinalizer = Ptr () -> IO ()

foreign import ccall "wrapper" createGlobalFinalizer :: GlobalFinalizer -> IO (FunPtr GlobalFinalizer)

foreign import ccall "wrapper" createFnWrapper :: (Ptr () -> IO ()) -> IO (FunPtr (Ptr () -> IO ()))

type DeviceInstancePtr = Ptr ()

type DeviceInitCallback = DeviceInstancePtr -> IO ()

foreign import ccall "wrapper" createDeviceInitCallback :: DeviceInitCallback -> IO (FunPtr DeviceInitCallback)

foreign import capi "c_tango.h tango_server_init"
  tango_server_init :: CInt -> Ptr CString -> FunPtr GlobalFinalizer -> CString -> CInt -> FunPtr DeviceInitCallback -> IO ()

foreign import capi "c_tango.h tango_server_start"
  tango_server_start :: IO ()

data HaskellAttributeDefinition = HaskellAttributeDefinition
  { attribute_name :: !CString,
    data_type :: !HaskellTangoDataType,
    write_type :: HaskellAttrWriteType,
    set_callback :: FunPtr (DeviceInstancePtr -> Ptr () -> IO ()),
    get_callback :: FunPtr (DeviceInstancePtr -> Ptr () -> IO ())
  }
  deriving (Show, Generic)

instance GStorable HaskellAttributeDefinition

type CommandCallbackInputPtr = Ptr ()

type CommandCallbackOutputPtr = Ptr ()

type CommandCallback = DeviceInstancePtr -> CommandCallbackInputPtr -> IO CommandCallbackOutputPtr

foreign import ccall "wrapper" createCommandCallback :: CommandCallback -> IO (FunPtr CommandCallback)

data HaskellCommandDefinition = HaskellCommandDefinition
  { command_name :: !CString,
    in_type :: !HaskellTangoDataType,
    out_type :: !HaskellTangoDataType,
    execute_callback :: FunPtr CommandCallback
  }
  deriving (Show, Generic)

instance GStorable HaskellCommandDefinition

foreign import capi "c_tango.h tango_server_add_attribute_definition"
  tango_server_add_attribute_definition :: Ptr HaskellAttributeDefinition -> IO ()

foreign import capi "c_tango.h tango_server_add_command_definition"
  tango_server_add_command_definition :: Ptr HaskellCommandDefinition -> IO ()

foreign import capi "c_tango.h tango_server_set_status"
  tango_server_set_status :: DeviceInstancePtr -> CString -> IO ()

foreign import capi "c_tango.h tango_server_set_state"
  tango_server_set_state :: DeviceInstancePtr -> CInt -> IO ()

foreign import capi "c_tango.h tango_server_add_property"
  tango_server_add_property :: CString -> IO ()

foreign import capi "c_tango.h tango_server_read_property"
  tango_server_read_property :: DeviceInstancePtr -> CString -> IO CString
