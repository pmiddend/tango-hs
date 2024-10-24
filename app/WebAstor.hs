{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative (pure)
import Data.Int (Int)
import Data.Text (Text, isPrefixOf, pack)
import Lucid (ToHtml (toHtml))
import Lucid.Html5 (h1_)
import Network.Wai.Handler.Warp (run)
import Servant (Get, Proxy (Proxy), QueryParam, (:>))
import Servant.API (JSON, Post, ReqBody)
import Servant.API.Generic (Generic)
import Servant.HTML.Lucid (HTML)
import Servant.Server (Application, Server, serve)
import System.Environment (getArgs)
import System.IO (IO, putStrLn)
import Prelude ()

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
