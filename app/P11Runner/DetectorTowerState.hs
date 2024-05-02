{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module P11Runner.DetectorTowerState where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import P11Runner.Markdown (Markdown, markdownBold)
import P11Runner.Util (numberIsCloseAbs)
import TangoHL (DeviceProxyPtr, HaskellTangoDevState (Moving, On, Running, Unknown), readDoubleAttribute, readStateAttribute, writeDoubleAttribute)

data DetectorTowerConfig = DetectorTowerConfig
  { towerConfigSafeDistanceMm :: Double,
    towerConfigMeasurementDistanceMm :: Double,
    towerConfigToleranceMm :: Double,
    towerConfigProxy :: DeviceProxyPtr
  }
  deriving (Eq, Show)

data DetectorTowerState
  = DetectorTowerMoving DetectorTowerConfig
  | DetectorTowerInSafe DetectorTowerConfig
  | DetectorTowerInMeasurement DetectorTowerConfig
  | DetectorTowerOtherPosition DetectorTowerConfig Double
  deriving (Eq, Show)

configFromState :: DetectorTowerState -> DetectorTowerConfig
configFromState (DetectorTowerMoving c) = c
configFromState (DetectorTowerInSafe c) = c
configFromState (DetectorTowerInMeasurement c) = c
configFromState (DetectorTowerOtherPosition c _) = c

updateConfigInState :: (DetectorTowerConfig -> DetectorTowerConfig) -> DetectorTowerState -> DetectorTowerState
updateConfigInState f (DetectorTowerMoving c) = DetectorTowerMoving (f c)
updateConfigInState f (DetectorTowerInSafe c) = DetectorTowerInSafe (f c)
updateConfigInState f (DetectorTowerInMeasurement c) = DetectorTowerInMeasurement (f c)
updateConfigInState f (DetectorTowerOtherPosition c p) = DetectorTowerOtherPosition (f c) p

detectorTowerIsMoving :: DetectorTowerState -> Bool
detectorTowerIsMoving (DetectorTowerMoving _) = True
detectorTowerIsMoving _ = False

detectorTowerInSafeDistance :: DetectorTowerState -> Bool
detectorTowerInSafeDistance (DetectorTowerInSafe _) = True
detectorTowerInSafeDistance _ = False

detectorTowerInMeasurementDistance :: DetectorTowerState -> Bool
detectorTowerInMeasurementDistance (DetectorTowerInMeasurement _) = True
detectorTowerInMeasurementDistance _ = False

detectorTowerInit :: (MonadIO m) => DetectorTowerConfig -> m DetectorTowerState
detectorTowerInit towerConfig = do
  tangoState <- liftIO $ readStateAttribute towerConfig.towerConfigProxy
  case tangoState of
    On -> do
      towerDistanceMm <- liftIO $ readDoubleAttribute towerConfig.towerConfigProxy "DetectorDistance"
      if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigMeasurementDistanceMm towerConfig.towerConfigToleranceMm
        then pure (DetectorTowerInMeasurement towerConfig)
        else
          if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigSafeDistanceMm towerConfig.towerConfigToleranceMm
            then pure (DetectorTowerInSafe towerConfig)
            else pure (DetectorTowerOtherPosition towerConfig towerDistanceMm)
    _otherState -> pure (DetectorTowerMoving towerConfig)

detectorTowerUpdate :: (MonadIO m) => DetectorTowerState -> m DetectorTowerState
detectorTowerUpdate currentState = do
  let towerConfig = configFromState currentState
  tangoState <- liftIO $ readStateAttribute towerConfig.towerConfigProxy
  case tangoState of
    On -> do
      towerDistanceMm <- liftIO $ readDoubleAttribute towerConfig.towerConfigProxy "DetectorDistance"
      if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigMeasurementDistanceMm towerConfig.towerConfigToleranceMm
        then pure (DetectorTowerInMeasurement towerConfig)
        else
          if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigSafeDistanceMm towerConfig.towerConfigToleranceMm
            then pure (DetectorTowerInSafe towerConfig)
            else pure (DetectorTowerOtherPosition towerConfig towerDistanceMm)
    _otherState -> pure (DetectorTowerMoving towerConfig)

detectorTowerMoveTo :: (MonadIO m) => Double -> DetectorTowerState -> m DetectorTowerState
detectorTowerMoveTo distance currentState = do
  let config = configFromState currentState
  liftIO $ writeDoubleAttribute config.towerConfigProxy "DetectorDistance" distance
  pure (DetectorTowerMoving config)

detectorTowerMoveToSafe :: (MonadIO m) => DetectorTowerState -> m (DetectorTowerState, Markdown)
detectorTowerMoveToSafe currentState = do
  let towerConfig = configFromState currentState
  towerDistanceMm <- liftIO $ readDoubleAttribute towerConfig.towerConfigProxy "DetectorDistance"
  if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigSafeDistanceMm towerConfig.towerConfigToleranceMm
    then pure (DetectorTowerInSafe towerConfig, markdownBold "tower" <> " already in safe distance")
    else do
      newState <- detectorTowerMoveTo towerConfig.towerConfigSafeDistanceMm currentState
      pure (newState, "moving " <> markdownBold "tower" <> " to safe distance")

detectorTowerMoveToMeasurement ::
  (MonadIO m) =>
  DetectorTowerState ->
  m
    ( DetectorTowerState,
      Markdown
    )
detectorTowerMoveToMeasurement currentState = do
  let towerConfig = configFromState currentState
  towerDistanceMm <- liftIO $ readDoubleAttribute towerConfig.towerConfigProxy "DetectorDistance"
  if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigMeasurementDistanceMm towerConfig.towerConfigToleranceMm
    then pure (DetectorTowerInMeasurement towerConfig, markdownBold "tower" <> " already in measurement distance")
    else do
      newState <- detectorTowerMoveTo towerConfig.towerConfigMeasurementDistanceMm currentState
      pure (newState, "moving " <> markdownBold "tower" <> " to measurement distance")

updateDetectorTowerMeasurementDistance :: DetectorTowerState -> Double -> DetectorTowerState
updateDetectorTowerMeasurementDistance state newDistance = updateConfigInState (\config -> config {towerConfigMeasurementDistanceMm = newDistance}) state

detectorTowerMeasurementDistance :: DetectorTowerState -> Double
detectorTowerMeasurementDistance = (.towerConfigMeasurementDistanceMm) . configFromState
