{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Int (Int16)
import Data.Text.IO qualified as TIO
import Tango.Client

data ScalarEnum = Label0 | Label1 | Label2 deriving (Enum, Show)

main =
  case tangoUrlFromText "sys/tg_test/1" of
    Left e -> error "couldn't resolve tango URL"
    Right deviceAddress -> withDeviceProxy deviceAddress \proxy -> do
      booleanResult <- readBoolAttribute proxy (AttributeName "boolean_scalar")
      putStrLn $ "boolean_scalar is " <> show booleanResult

      booleanResultBetter <- readBoolAttribute proxy (AttributeName "boolean_scalar")
      putStrLn $ "boolean_scalar better is " <> show booleanResultBetter

      -- booleanSpectrumResult <- readBoolSpectrumAttribute proxy (AttributeName "boolean_spectrum")
      -- putStrLn $ "boolean_spectrum is " <> show booleanSpectrumResult
      booleanSpectrumResultBetter <- readBoolSpectrumAttribute proxy (AttributeName "boolean_spectrum")
      putStrLn $ "boolean_spectrum is " <> show booleanSpectrumResultBetter

      -- booleanImageResult <- readBoolImageAttribute proxy (AttributeName "boolean_image")
      -- putStrLn $ "boolean_image is " <> show booleanImageResult

      enumResult :: TangoValue Int16 <- readEnumAttribute proxy (AttributeName "enum_scalar")
      putStrLn $ "enum_scalar is " <> show enumResult

      attributeList <- getConfigsForAttributes proxy [AttributeName "enum_scalar_ro"]
      putStrLn $ "attribute description for \"enum_scalar_ro\": " <> show attributeList

      (TangoValue enumResultRead' enumResultWrite') <- readEnumAttribute proxy (AttributeName "enum_scalar")
      putStrLn $ "enum_scalar (as Haskell enum) is " <> show (enumResultRead' :: ScalarEnum)

      -- stringImage <- readStringImageAttribute proxy (AttributeName "string_image_ro")
      -- putStrLn "string image contents follow:"
      -- mapM_ TIO.putStrLn (imageContent stringImage)
      -- putStrLn "string image end"

      writeLong64Attribute proxy (AttributeName "long64_scalar") 1337

      result <- commandInOutGeneric proxy (CommandName "DevVoid") CommandVoid
      putStrLn $ "result of DevVoid command: " <> show result
