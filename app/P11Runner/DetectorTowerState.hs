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

data DetectorTowerState
  = DetectorTowerMoving
  | DetectorTowerInSafe
  | DetectorTowerInMeasurement
  | DetectorTowerOtherPosition Double
  deriving (Eq, Show)

detectorTowerIsMoving :: DetectorTowerState -> Bool
detectorTowerIsMoving x = x == DetectorTowerMoving

detectorTowerInSafeDistance :: DetectorTowerState -> Bool
detectorTowerInSafeDistance x = x == DetectorTowerInSafe

detectorTowerInMeasurementDistance :: DetectorTowerState -> Bool
detectorTowerInMeasurementDistance x = x == DetectorTowerInMeasurement

data DetectorTowerConfig = DetectorTowerConfig
  { towerConfigSafeDistanceMm :: Double,
    towerConfigMeasurementDistanceMm :: Double,
    towerConfigToleranceMm :: Double
  }

detectorTowerUpdate :: (MonadIO m) => DeviceProxyPtr -> DetectorTowerConfig -> m DetectorTowerState
detectorTowerUpdate towerProxy towerConfig = do
  tangoState <- liftIO $ readStateAttribute towerProxy
  case tangoState of
    On -> do
      towerDistanceMm <- liftIO $ readDoubleAttribute towerProxy "DetectorDistance"
      if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigMeasurementDistanceMm towerConfig.towerConfigToleranceMm
        then pure DetectorTowerInMeasurement
        else
          if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigSafeDistanceMm towerConfig.towerConfigToleranceMm
            then pure DetectorTowerInSafe
            else pure (DetectorTowerOtherPosition towerDistanceMm)
    _otherState -> pure DetectorTowerMoving

detectorTowerMoveTo :: (MonadIO m) => DeviceProxyPtr -> Double -> m DetectorTowerState
detectorTowerMoveTo towerProxy distance = do
  liftIO $ writeDoubleAttribute towerProxy "DetectorDistance" distance
  pure DetectorTowerMoving

detectorTowerMoveToSafe :: (MonadIO m) => DeviceProxyPtr -> DetectorTowerConfig -> m (DetectorTowerState, Markdown)
detectorTowerMoveToSafe towerProxy towerConfig = do
  towerDistanceMm <- liftIO $ readDoubleAttribute towerProxy "DetectorDistance"
  if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigSafeDistanceMm towerConfig.towerConfigToleranceMm
    then pure (DetectorTowerInSafe, markdownBold "tower" <> " already in safe distance")
    else do
      newState <- detectorTowerMoveTo towerProxy towerConfig.towerConfigSafeDistanceMm
      pure (newState, "moving " <> markdownBold "tower" <> " to safe distance")

detectorTowerMoveToMeasurement ::
  (MonadIO m) =>
  DeviceProxyPtr ->
  DetectorTowerConfig ->
  m
    ( DetectorTowerState,
      Markdown
    )
detectorTowerMoveToMeasurement towerProxy towerConfig = do
  towerDistanceMm <- liftIO $ readDoubleAttribute towerProxy "DetectorDistance"
  if numberIsCloseAbs towerDistanceMm towerConfig.towerConfigMeasurementDistanceMm towerConfig.towerConfigToleranceMm
    then pure (DetectorTowerInMeasurement, markdownBold "tower" <> " already in measurement distance")
    else do
      newState <- detectorTowerMoveTo towerProxy towerConfig.towerConfigMeasurementDistanceMm
      pure (newState, "moving " <> markdownBold "tower" <> " to measurement distance")
