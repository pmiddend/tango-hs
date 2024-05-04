{-# LANGUAGE OverloadedStrings #-}

module P11Runner.Util where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Prelude hiding (putStrLn)

numberIsClose :: (Ord a, Num a) => a -> a -> a -> a -> Bool
numberIsClose a b relTol absTol =
  abs (a - b) <= max (relTol * max (abs a) (abs b)) absTol

numberIsCloseAbs :: (Ord a, Fractional a) => a -> a -> a -> Bool
numberIsCloseAbs a b = numberIsClose a b 1e-9

logToConsole :: (MonadIO m) => Text -> m ()
logToConsole t = do
  currentTime <- liftIO getCurrentTime
  liftIO $ putStrLn $ pack (iso8601Show currentTime) <> " " <> t
