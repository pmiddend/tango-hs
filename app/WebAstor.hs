{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative (pure)
import Control.Exception (handle)
import Control.Monad (forM_, (>>=))
import Control.Monad.IO.Class (liftIO)
import Data.Bool (not)
import Data.Either (Either (Left, Right))
import Data.Function (flip, ($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, breakOn, breakOnAll, drop, isPrefixOf, lines, pack, strip, unlines, words)
import Data.Text.Read (decimal)
import Data.Traversable (for, traverse)
import Data.Typeable (Typeable)
import Lucid (ToHtml (toHtml, toHtmlRaw))
import Lucid qualified as L
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
import Tango.Client (AttributeName (AttributeName), TangoException (TangoException), parseTangoUrl, readStringSpectrumAttribute, tangoValueRead, withDeviceProxy)
import Text.Show (show)
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

data RefreshOutput = RefreshOutput
  { refreshOutputServers :: Either Text [AstorServer],
    refreshOutputHost :: Text
  }
  deriving (Generic)

instance ToHtml RefreshOutput where
  toHtml (RefreshOutput servers host) = viewHtmlSkeleton do
    L.form_ [L.action_ "servers"] do
      L.div_ [L.class_ "form-floating mb-3"] do
        L.input_
          [ L.type_ "text",
            L.class_ "form-control",
            L.id_ "host",
            L.placeholder_ "localhost:10000",
            L.name_ "host",
            L.value_ host
          ]
        L.label_ [L.for_ "host"] "Tango Host:Port"
      L.button_ [L.class_ "btn btn-primary mb-3", L.type_ "submit"] "Refresh"
    case servers of
      Left e -> L.div_ [L.class_ "alert alert-danger"] (L.toHtml ("Something went wrong: " <> e))
      Right serverList -> L.table_ [L.class_ "table"] do
        L.tbody_
          ( forM_ serverList \(AstorServer {serverDevice, serverStatus}) ->
              L.tr_ do
                L.td_ (L.code_ (L.toHtml serverDevice))
                L.td_ (L.toHtml serverStatus)
          )
  toHtmlRaw = toHtml

type AstorAPI = "servers" :> QueryParam "host" Text :> Get '[HTML] RefreshOutput

parseServersOutput :: Text -> Either Text [AstorServer]
parseServersOutput l = for (lines l) \line -> case words line of
  [device, status, controlledStr, levelStr, nbInstancesStr] ->
    case (decimal controlledStr, decimal levelStr, decimal nbInstancesStr) of
      (Right (controlled, _), Right (level, _), Right (nbInstances, _)) ->
        Right (AstorServer {serverDevice = device, serverStatus = status, serverControlled = controlled, serverLevel = level, serverNbInstances = nbInstances})
      _ -> Left ("one of controlled, level, nbInstances is not a number: " <> line)
  _ -> Left ("expected whitespace-separated list of 5 values, got: " <> line)

data HostWithPort = HostWithPort Text Int

parseHostWithPort :: Text -> Maybe HostWithPort
parseHostWithPort hostAndPort =
  case breakOn ":" hostAndPort of
    (host, maybePort) ->
      if not (":" `isPrefixOf` maybePort)
        then Nothing
        else case decimal (drop 1 maybePort) of
          Right (port, _) -> Just (HostWithPort host port)
          _ -> Nothing

astorServer :: Server AstorAPI
astorServer inputHost =
  case inputHost >>= parseHostWithPort of
    Nothing ->
      pure
        ( RefreshOutput
            { refreshOutputHost = "",
              refreshOutputServers = Right []
            }
        )
    Just (HostWithPort inputHost' inputPort) ->
      let inputHostAndPortStr = inputHost' <> ":" <> pack (show inputPort)
       in case parseTangoUrl ("tango://" <> inputHostAndPortStr <> "/tango/admin/" <> inputHost') of
            Left e ->
              pure
                ( RefreshOutput
                    { refreshOutputHost = inputHostAndPortStr,
                      refreshOutputServers = Left ("error parsing host: " <> e)
                    }
                )
            Right deviceAddress -> liftIO
              $ handle
                ( \e@(TangoException _) ->
                    pure
                      ( RefreshOutput
                          { refreshOutputHost = inputHostAndPortStr,
                            refreshOutputServers = Left ("error in tango: " <> pack (show e))
                          }
                      )
                )
              $ withDeviceProxy deviceAddress \proxy -> do
                servers <- readStringSpectrumAttribute proxy (AttributeName "Servers")
                case parseServersOutput (unlines (tangoValueRead servers)) of
                  Left e ->
                    pure
                      ( RefreshOutput
                          { refreshOutputHost = inputHostAndPortStr,
                            refreshOutputServers = Left ("error parsing servers output: " <> e)
                          }
                      )
                  Right serversParsed ->
                    pure
                      ( RefreshOutput
                          { refreshOutputHost = inputHostAndPortStr,
                            refreshOutputServers = Right serversParsed
                          }
                      )

astorAPI :: Proxy AstorAPI
astorAPI = Proxy

app :: Application
app = serve astorAPI astorServer

viewHtmlSkeleton content = do
  L.doctypehtml_ do
    L.head_ do
      L.meta_ [L.charset_ "utf-8"]
      L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
      L.title_ "Astor on the Web"
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css", L.rel_ "stylesheet"]
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css", L.rel_ "stylesheet"]
    L.body_ do
      L.div_ [L.class_ "container"] do
        L.header_ do
          L.h1_ "Astor on the Web"
        L.main_ do
          content
      -- Not sure if we need the bootstrap JS, and it must save some bandwidth, so leave it out maybe
      L.script_ [L.src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"] ("" :: Text)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--generate-openapi"] -> do
      pure ()
    _ -> do
      putStrLn "listening on port 8081..."
      run 8081 app
