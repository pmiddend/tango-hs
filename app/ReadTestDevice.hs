{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tango.Client

main =
  case tangoUrlFromText "sys/tg_test/1" of
    Left e -> error "couldn't resolve tango URL"
    Right deviceAddress -> withDeviceProxy deviceAddress \proxy -> do
      booleanResult <- readBoolAttribute proxy (AttributeName "boolean_scalar")
      putStrLn $ "boolean_scalar is " <> show booleanResult

      booleanSpectrumResult <- readBoolSpectrumAttribute proxy (AttributeName "boolean_spectrum")
      putStrLn $ "boolean_spectrum is " <> show booleanSpectrumResult

      booleanImageResult <- readBoolImageAttribute proxy (AttributeName "boolean_image")
      putStrLn $ "boolean_image is " <> show booleanImageResult

      enumResult <- readShortAttribute proxy (AttributeName "enum_scalar")
      putStrLn $ "enum_scalar is " <> show enumResult

      attributeList <- getConfigsForAttributes proxy [AttributeName "enum_scalar_ro"]

      print attributeList
