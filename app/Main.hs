{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foreign (Storable (peek), alloca)
import Foreign.C.String (withCString)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Tango

main :: IO ()
main = do
  putStrLn "creating proxy"
  alloca $ \proxyPtrPtr -> withCString "sys/tg_test/1" $ \proxyName -> do
    result <- tango_create_device_proxy proxyName proxyPtrPtr
    proxyPtr <- peek proxyPtrPtr
    putStrLn ("got proxy " <> show result <> " lol")
    withCString "double_scalar" $ \attributeName -> do
      hPutStrLn stderr "before with"
      -- alloca $ \argout -> do
      alloca $ \argoutPtr -> do
        hPutStrLn stderr "reading attribute"
        hFlush stdout
        attrReadResult <- tango_read_attribute proxyPtr attributeName argoutPtr
        argout' <- peek argoutPtr
        putStrLn ("read attribute" <> show attrReadResult)
        putStrLn ("result " <> show argout')
