module Main where

import Tango

main :: IO ()
main = do
  putStrLn "creating proxy"
  unsafePerformIO $ alloca $ \proxyPtr -> do
    result <- tango_create_device_proxy "foo" proxyPtr
    putStrLn "done"
