{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO qualified as TIO
import Tango.Client

data ScalarEnum = Label0 | Label1 | Label2 deriving (Enum, Show)

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
      putStrLn $ "attribute description for \"enum_scalar_ro\": " <> show attributeList

      enumResult' <- readEnumAttribute proxy (AttributeName "enum_scalar")
      putStrLn $ "enum_scalar (as Haskell enum) is " <> show (enumResult' :: ScalarEnum)

      stringImage <- readStringImageAttribute proxy (AttributeName "string_image_ro")
      putStrLn "string image contents follow:"
      mapM_ TIO.putStrLn (imageContent stringImage)
      putStrLn "string image end"
