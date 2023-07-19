{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign (alloca)
import Foreign.C.String (withCString)
import Tango

main :: IO ()
main = do
  putStrLn "creating proxy"
  alloca $ \proxyPtr -> withCString "sys/tg_test/1" $ \proxyName -> do
    result <- tango_create_device_proxy proxyName proxyPtr
    putStrLn "done"
