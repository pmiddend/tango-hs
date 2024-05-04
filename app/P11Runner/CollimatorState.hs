{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module P11Runner.CollimatorState where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), eitherDecode, eitherDecodeFileStrict', eitherDecodeStrict, object, withObject, (.:))
import Data.Fixed (Pico)
import Data.Monoid (All (getAll), Any (Any, getAny))
import Data.Semigroup (All (All))
import Data.Text (Text, pack, unpack)
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock (UTCTime, diffUTCTime)
import P11Runner.Markdown (Markdown, markdownBold)
import P11Runner.Util (logToConsole, numberIsCloseAbs)
import TangoHL (DeviceProxyPtr, HaskellTangoDevState (Moving, On, Running, Unknown), readDoubleAttribute, readStateAttribute, writeDoubleAttribute)

data ColliAxisValues a = ColliAxisValues
  { yValue :: a,
    zValue :: a
  }
  deriving (Functor, Foldable, Traversable, Show)

instance Applicative ColliAxisValues where
  liftA2 f (ColliAxisValues a0 a1) (ColliAxisValues b0 b1) = ColliAxisValues (f a0 b0) (f a1 b1)
  pure x = ColliAxisValues x x

zipWith :: (a -> b -> c) -> ColliAxisValues a -> ColliAxisValues b -> ColliAxisValues c
zipWith f (ColliAxisValues a0 a1) (ColliAxisValues b0 b1) = ColliAxisValues (f a0 b0) (f a1 b1)

zipWithM :: (Applicative m) => (a -> b -> m c) -> ColliAxisValues a -> ColliAxisValues b -> m (ColliAxisValues c)
zipWithM f (ColliAxisValues a0 a1) (ColliAxisValues b0 b1) = ColliAxisValues <$> f a0 b0 <*> f a1 b1

data StaticColliConfig = StaticColliConfig
  { inPositions :: ColliAxisValues Double,
    outPositions :: ColliAxisValues Double,
    tolerances :: ColliAxisValues Double,
    movementDurationS :: NominalDiffTime,
    proxies :: ColliAxisValues DeviceProxyPtr
  }
  deriving (Show)

data ColliConfigJson = ColliConfigJson
  { inY :: Double,
    inZ :: Double,
    outY :: Double,
    outZ :: Double
  }

instance FromJSON ColliConfigJson where
  parseJSON = withObject "ColliConfigJson" \v ->
    ColliConfigJson
      <$> v .: "in_y"
      <*> v .: "in_z"
      <*> v .: "out_y"
      <*> v .: "out_z"

readColliConfigJson :: (MonadIO m) => FilePath -> m (Either Text ColliConfigJson)
readColliConfigJson fp =
  liftIO $
    eitherDecodeFileStrict' fp >>= \case
      Left e -> pure $ Left (pack e)
      Right v -> pure (Right v)

staticColliConfigFromJson :: ColliConfigJson -> Double -> ColliAxisValues Double -> ColliAxisValues DeviceProxyPtr -> StaticColliConfig
staticColliConfigFromJson (ColliConfigJson {inY, inZ, outY, outZ}) movementDurationS tolerances proxies =
  StaticColliConfig
    { inPositions = ColliAxisValues inY inZ,
      outPositions = ColliAxisValues outY outZ,
      tolerances = tolerances,
      movementDurationS = secondsToNominalDiffTime (fromIntegral @Int @Pico $ round movementDurationS),
      proxies = proxies
    }

data DesiredStatus = DesiredIn | DesiredOut deriving (Eq, Show)

data ColliState = ColliState
  { static :: StaticColliConfig,
    desiredStatus :: DesiredStatus,
    movementBeginning :: Maybe UTCTime
  }
  deriving (Show)

colliUpdateDesiredPosition :: ColliState -> DesiredStatus -> ColliState
colliUpdateDesiredPosition s d = s {desiredStatus = d}

colliMove :: (MonadIO m) => ColliState -> ColliAxisValues Double -> m (ColliState, Markdown)
colliMove state positions = do
  void $
    liftIO $
      zipWithM
        (`writeDoubleAttribute` "Position")
        state.static.proxies
        positions
  currentTime <- liftIO getCurrentTime
  pure (state {movementBeginning = Just currentTime}, "moving " <> markdownBold "collimator" <> ", this might take a while")

colliMoveToDesired :: (MonadIO m) => ColliState -> m (ColliState, Markdown)
colliMoveToDesired state = do
  inDesired <- isColliInDesired state
  if inDesired
    then pure (state, markdownBold "collimator" <> " already in desired state")
    else colliMove state (if state.desiredStatus == DesiredIn then state.static.inPositions else state.static.outPositions)

colliMoveOut :: (MonadIO m) => ColliState -> m (ColliState, Markdown)
colliMoveOut state = colliMove state state.static.outPositions

colliInit :: (MonadIO m) => Text -> Double -> ColliAxisValues Double -> ColliAxisValues DeviceProxyPtr -> m (Either Text ColliState)
colliInit colliConfigFile movementDurationS tolerances proxies = do
  colliConfig <- readColliConfigJson (unpack colliConfigFile)
  pure $ case colliConfig of
    Left e -> Left $ "invalid colli config file \"" <> colliConfigFile <> "\": " <> e
    Right colliConfig' -> do
      Right $
        ColliState
          { static = staticColliConfigFromJson colliConfig' movementDurationS tolerances proxies,
            desiredStatus = DesiredIn,
            movementBeginning = Nothing
          }

isColliCloseTo :: (MonadIO m) => ColliState -> ColliAxisValues Double -> m Bool
isColliCloseTo state askedPositions = do
  positions :: ColliAxisValues Double <- liftIO $ traverse (`readDoubleAttribute` "Position") state.static.proxies
  let inAsked :: ColliAxisValues Bool
      inAsked = numberIsCloseAbs <$> positions <*> askedPositions <*> state.static.tolerances
  pure $ getAll $ foldMap All inAsked

isColliInDesired :: (MonadIO m) => ColliState -> m Bool
isColliInDesired state = isColliCloseTo state (if state.desiredStatus == DesiredIn then state.static.inPositions else state.static.outPositions)

isColliMoving :: (MonadIO m) => ColliState -> m Bool
isColliMoving state = do
  states :: ColliAxisValues HaskellTangoDevState <- liftIO $ traverse readStateAttribute state.static.proxies
  currentTime <- liftIO getCurrentTime
  let isMoving = getAny $ foldMap (Any . (== Moving)) states
  case state.movementBeginning of
    Nothing -> pure isMoving
    Just movementBeginning' -> do
      logToConsole $ "movement beginning, diff: " <> pack (show (currentTime `diffUTCTime` movementBeginning'))
      pure (isMoving || (currentTime `diffUTCTime` movementBeginning' < state.static.movementDurationS))

isColliOut :: (MonadIO m) => ColliState -> m Bool
isColliOut state = isColliCloseTo state state.static.outPositions
