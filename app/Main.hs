{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void, when)
import qualified Data.Vector.Storable as V
import Foreign
  ( Storable (peek),
    alloca,
  )
import Foreign.C.String (withCString)
import Foreign.Marshal (with)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Tango
  ( HaskellAttributeData (HaskellAttributeData),
    HaskellCommandData (HaskellCommandData),
    HaskellErrorStack,
    HaskellTangoCommandData (HaskellCommandDouble, HaskellCommandString),
    HaskellTangoDataType (HaskellDevDouble, HaskellDevString),
    haskellDevSourceDev,
    newDoubleArray,
    stringToVector,
    tango_command_inout,
    tango_create_device_proxy,
    tango_delete_device_proxy,
    tango_free_AttributeData,
    tango_free_CommandData,
    tango_get_source,
    tango_get_timeout_millis,
    tango_is_locked,
    tango_is_locked_by_me,
    tango_lock,
    tango_read_attribute,
    tango_set_source,
    tango_set_timeout_millis,
    tango_unlock,
    tango_write_attribute,
  )

checkResult :: IO (Ptr HaskellErrorStack) -> IO ()
checkResult action = do
  es <- action
  when (es /= nullPtr) (fail "error")

main :: IO ()
main = do
  putStrLn "creating proxy"
  alloca $ \proxyPtrPtr -> withCString "sys/tg_test/1" $ \proxyName -> do
    checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
    proxyPtr <- peek proxyPtrPtr
    putStrLn "got proxy"

    putStrLn "setting timeout to 1337ms"
    checkResult (tango_set_timeout_millis proxyPtr 1337)
    alloca $ \millisPtr -> do
      checkResult (tango_get_timeout_millis proxyPtr millisPtr)
      millis <- peek millisPtr
      putStrLn ("millis are " <> show millis)

    putStrLn "locking"
    checkResult (tango_lock proxyPtr)

    putStrLn "locked?"
    alloca $ \boolPtr -> do
      checkResult (tango_is_locked proxyPtr boolPtr)
      bool <- peek boolPtr
      print bool
      putStrLn "locked by me?"
      checkResult (tango_is_locked_by_me proxyPtr boolPtr)
      bool <- peek boolPtr
      print bool

    putStrLn "unlocking"
    checkResult (tango_unlock proxyPtr)

    putStrLn "locked after unlocking?"
    alloca $ \boolPtr -> do
      checkResult (tango_is_locked proxyPtr boolPtr)
      bool <- peek boolPtr
      print bool

    putStrLn "setting source"
    checkResult (tango_set_source proxyPtr haskellDevSourceDev)

    putStrLn "getting source"
    alloca $ \sourcePtr -> do
      checkResult (tango_get_source proxyPtr sourcePtr)
      source <- peek sourcePtr
      putStrLn ("source is " <> show source)

    with (HaskellCommandData HaskellDevDouble (HaskellCommandDouble 3.0)) $ \arginPtr ->
      alloca $ \argoutPtr -> withCString "DevDouble" $ \cmdName -> do
        hPutStrLn stderr "executing command"
        commandResult <- tango_command_inout proxyPtr cmdName arginPtr argoutPtr
        hPutStrLn stderr ("executed command: " <> show commandResult)
        argOut <- peek argoutPtr
        hPutStrLn stderr ("result: " <> show argOut)
        tango_free_CommandData argoutPtr

    with (HaskellCommandData HaskellDevString (HaskellCommandString (V.fromList [115, 116, 114, 105, 110, 103, 95, 115, 99, 97, 108, 97, 114]))) $ \arginPtr ->
      alloca $ \argoutPtr -> withCString "DevString" $ \cmdName -> do
        hPutStrLn stderr "executing command"
        commandResult <- tango_command_inout proxyPtr cmdName arginPtr argoutPtr
        hPutStrLn stderr ("executed command: " <> show commandResult)
        argOut <- peek argoutPtr
        hPutStrLn stderr ("result: " <> show argOut)

    withCString "double_scalar" $ \attributeName -> do
      hPutStrLn stderr "before with"
      alloca $ \argoutPtr -> do
        hPutStrLn stderr "reading attribute"
        hFlush stdout
        attrReadResult <- tango_read_attribute proxyPtr attributeName argoutPtr
        argout' <- peek argoutPtr
        putStrLn ("read attribute " <> show attrReadResult)
        putStrLn ("result " <> show argout')
        tango_free_AttributeData argoutPtr

      with (HaskellAttributeData undefined undefined undefined (stringToVector "double_scalar") 1 0 undefined HaskellDevDouble (newDoubleArray [1338.0])) $ \argoutPtr -> do
        hPutStrLn stderr "writing attribute"
        attrWriteResult <- tango_write_attribute proxyPtr argoutPtr
        _ <- peek argoutPtr
        putStrLn ("read attribute " <> show attrWriteResult)

    void (tango_delete_device_proxy proxyPtr)
