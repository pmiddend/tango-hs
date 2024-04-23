module P11States where

data DetectorTowerState
  = DetectorTowerMoving
  | DetectorTowerInSafe
  | DetectorTowerInMeasurement
  | DetectorTowerOtherPosition Double

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
      if numberIsCloseAbs towerDistanceMm towerConfig . towerConfigMeasurementDistanceMm towerConfig . towerConfigToleranceMm
        then pure DetectorTowerInMeasurement
        else
          if numberIsCloseAbs towerDistanceMm towerConfig . towerConfigSafeDistanceMm towerConfig . towerConfigToleranceMm
            then pure DetectorTowerInSafe
            else pure (DetectorTowerOtherPosition towerDistanceMm)
    _otherState -> pure DetectorTowerMoving

detectorTowerMoveTo :: (MonadIO m) => DeviceProxyPtr -> Double -> m DetectorTowerState
detectorTowerMoveTo towerProxy distance = do
  liftIO $ writeDoubleAttribute towerProxy "DetectorDistance" distance
  pure DetectorTowerMoving

detectorTowerMoveToSafe :: (MonadIO m) => DeviceProxyPtr -> DetectorTowerConfig -> m DetectorTowerState
detectorTowerMoveToSafe towerProxy towerConfig = detectorTowerMoveTo towerProxy towerConfig . towerConfigSafeDistanceMm

detectorTowerMoveToMeasurement :: (MonadIO m) => DeviceProxyPtr -> DetectorTowerConfig -> m DetectorTowerState
detectorTowerMoveToMeasurement towerProxy towerConfig = detectorTowerMoveTo towerProxy towerConfig . towerConfigMeasurementDistanceMm

data ColliFinishedData = ColliFinishedData
  { colliFinishedInDesired :: Bool,
    colliFinishedOut :: Bool
  }

data ColliMovingData = ColliMovingData
  { colliMovingEndTime :: Maybe UTCTime,
    colliMovingAxes :: NE.NonEmpty Text
  }

data ColliState
  = ColliFinished ColliFinishedData
  | ColliMoving ColliMovingData

colliUpdate ::
  DeviceProxyPtr ->
  DeviceProxyPtr ->
  ColliConfig ->
  DesiredCollimatorStatus ->
  ColliState ->
  P11RunnerMonad ColliState
colliUpdate collimatorY collimatorZ colliConfig desiredStatus priorState = do
  let innerUpdate movementEnd = do
        collimatorYState <- liftIO $ readStateAttribute collimatorY
        collimatorZState <- liftIO $ readStateAttribute collimatorZ
        case (collimatorYState, collimatorZState) of
          (Moving, Moving) -> pure (ColliMoving (ColliMovingData {colliMovingEndTime = movementEnd, colliMovingAxes = "y" NE.:| ["z"]}))
          (Moving, _) -> pure (ColliMoving (ColliMovingData {colliMovingEndTime = movementEnd, colliMovingAxes = NE.singleton "y"}))
          (_, Moving) -> pure (ColliMoving (ColliMovingData {colliMovingEndTime = movementEnd, colliMovingAxes = NE.singleton "z"}))
          _ -> do
            yInDesired <- checkColliMotor collimatorY colliConfig . inY colliConfig . outY colliConfig . toleranceY desiredStatus
            zInDesired <- checkColliMotor collimatorZ colliConfig . inZ colliConfig . outZ colliConfig . toleranceZ desiredStatus
            yOut <- checkColliMotor collimatorY colliConfig . inY colliConfig . outY colliConfig . toleranceY DesiredOut
            zOut <- checkColliMotor collimatorZ colliConfig . inZ colliConfig . outZ colliConfig . toleranceZ DesiredOut
            let bothInDesired = yInDesired && zInDesired
                bothOut = yOut && zOut
            pure (ColliFinished (ColliFinishedData {colliFinishedInDesired = bothInDesired, colliFinishedOut = bothOut}))
  currentTime <- liftIO getCurrentTime
  case priorState of
    (ColliMoving (ColliMovingData {colliMovingEndTime})) ->
      if maybe False (currentTime <) colliMovingEndTime
        then do
          logToConsole "collimator timeout not expired yet, waiting"
          pure priorState
        else innerUpdate colliMovingEndTime
    _ -> innerUpdate Nothing

colliMoveOut ::
  DeviceProxyPtr ->
  DeviceProxyPtr ->
  ColliConfig ->
  P11RunnerMonad ColliState
colliMoveOut collimatorY collimatorZ colliConfig = do
  liftIO $ writeDoubleAttribute collimatorY "Position" colliConfig . outY
  liftIO $ writeDoubleAttribute collimatorZ "Position" colliConfig . outZ
  currentTime <- liftIO getCurrentTime
  pure (ColliMoving (ColliMovingData {colliMovingEndTime = Just (addUTCTime colliConfig . movementTimeoutS currentTime), colliMovingAxes = "y" NE.:| ["z"]}))

colliMoveToDesired ::
  DeviceProxyPtr ->
  DeviceProxyPtr ->
  ColliConfig ->
  DesiredCollimatorStatus ->
  P11RunnerMonad ColliState
colliMoveToDesired collimatorY collimatorZ colliConfig desired = do
  let positionY = if desired == DesiredIn then colliConfig . inY else colliConfig . outY
      positionZ = if desired == DesiredIn then colliConfig . inZ else colliConfig . outZ
  liftIO $ writeDoubleAttribute collimatorY "Position" positionY
  liftIO $ writeDoubleAttribute collimatorZ "Position" positionZ
  currentTime <- liftIO getCurrentTime
  pure (ColliMoving (ColliMovingData {colliMovingEndTime = Just (addUTCTime colliConfig . movementTimeoutS currentTime), colliMovingAxes = "y" NE.:| ["z"]}))
