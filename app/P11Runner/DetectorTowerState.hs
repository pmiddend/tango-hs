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
  = DetectorTowerMoving
  | DetectorTowerInSafe
  | DetectorTowerInMeasurement
  | DetectorTowerOtherPosition Double
  deriving (Eq, Show)

calculateDetectorTowerState :: (MonadIO m) => DetectorTowerConfig -> m DetectorTowerState
calculateDetectorTowerState config = do
  tangoState <- liftIO $ readStateAttribute config.towerConfigProxy
  if tangoState == Moving
    then pure DetectorTowerMoving
    else do
      towerDistanceMm <- liftIO $ readDoubleAttribute config.towerConfigProxy "DetectorDistance"
      if numberIsCloseAbs towerDistanceMm config.towerConfigMeasurementDistanceMm config.towerConfigToleranceMm
        then pure DetectorTowerInMeasurement
        else
          if numberIsCloseAbs towerDistanceMm config.towerConfigSafeDistanceMm config.towerConfigToleranceMm
            then pure DetectorTowerInSafe
            else pure (DetectorTowerOtherPosition towerDistanceMm)

detectorTowerMoveTo :: (MonadIO m) => Double -> DetectorTowerConfig -> m ()
detectorTowerMoveTo distance config = do
  liftIO $ writeDoubleAttribute config.towerConfigProxy "DetectorDistance" distance

detectorTowerMoveToSafe :: (MonadIO m) => DetectorTowerConfig -> m Markdown
detectorTowerMoveToSafe towerConfig = do
  towerDistanceMm <- liftIO $ readDoubleAttribute towerConfig.towerConfigProxy "DetectorDistance"
  if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigSafeDistanceMm towerConfig.towerConfigToleranceMm
    then pure $ markdownBold "tower" <> " already in safe distance"
    else do
      newState <- detectorTowerMoveTo towerConfig.towerConfigSafeDistanceMm towerConfig
      pure $ "moving " <> markdownBold "tower" <> " to safe distance"

detectorTowerMoveToMeasurement :: (MonadIO m) => DetectorTowerConfig -> m Markdown
detectorTowerMoveToMeasurement towerConfig = do
  towerDistanceMm <- liftIO $ readDoubleAttribute towerConfig.towerConfigProxy "DetectorDistance"
  if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigMeasurementDistanceMm towerConfig.towerConfigToleranceMm
    then pure $ markdownBold "tower" <> " already in measurement distance"
    else do
      newState <- detectorTowerMoveTo towerConfig.towerConfigMeasurementDistanceMm towerConfig
      pure $ "moving " <> markdownBold "tower" <> " to measurement distance"

updateDetectorTowerMeasurementDistance :: DetectorTowerConfig -> Double -> DetectorTowerConfig
updateDetectorTowerMeasurementDistance config newDistance = config {towerConfigMeasurementDistanceMm = newDistance}

detectorTowerMeasurementDistance :: DetectorTowerConfig -> Double
detectorTowerMeasurementDistance config = config.towerConfigMeasurementDistanceMm
