{-# LANGUAGE OverloadedStrings #-}

module P11Runner.Markdown where

import Data.String (IsString)
import Data.Text (Text)

type Markdown = Text

markdownPlain :: Text -> Text
markdownPlain = id

markdownBold :: (Semigroup a, IsString a) => a -> a
markdownBold x = "**" <> x <> "**"

markdownEmph :: (Semigroup a, IsString a) => a -> a
markdownEmph x = "*" <> x <> "*"
