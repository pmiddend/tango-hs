{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative (pure)
import Data.Function ((.))
import Data.Int (Int)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, isPrefixOf, pack)
import Data.Typeable (Typeable)
import Lucid (ToHtml (toHtml, toHtmlRaw))
import Lucid.Base (renderBS)
import Lucid.Html5 (h1_)
import Network.HTTP.Media qualified as M
import Network.Wai.Handler.Warp (run)
import Servant (Get, Proxy (Proxy), QueryParam, (:>))
import Servant.API (Accept (..), JSON, MimeRender (..), Post, ReqBody)
import Servant.API.Generic (Generic)
import Servant.Server (Application, Server, serve)
import System.Environment (getArgs)
import System.IO (IO, putStrLn)
import Prelude ()

data HTML deriving stock (Typeable)

instance Accept HTML where
  contentTypes _ =
    "text" M.// "html" M./: ("charset", "utf-8")
      NE.:| ["text" M.// "html"]

instance (ToHtml a) => MimeRender HTML a where
  mimeRender _ = renderBS . toHtml

data AstorServer = AstorServer
  { serverDevice :: Text,
    serverStatus :: Text,
    serverControlled :: Int,
    serverLevel :: Int,
    serverNbInstances :: Int
  }
  deriving (Generic)

newtype RefreshOutput = RefreshOutput {refreshOutputServers :: [AstorServer]} deriving (Generic)

instance ToHtml RefreshOutput where
  toHtml (RefreshOutput servers) = h1_ "foo"
  toHtmlRaw = toHtml

type AstorAPI = "servers" :> QueryParam "url" Text :> Get '[HTML] RefreshOutput

astorServer :: Server AstorAPI
astorServer inputUrl = do
  pure (RefreshOutput [])

astorAPI :: Proxy AstorAPI
astorAPI = Proxy

app :: Application
app = serve astorAPI astorServer

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--generate-openapi"] -> do
      pure ()
    _ -> do
      putStrLn "listening on port 8081..."
      run 8081 app
