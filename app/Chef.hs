{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AmarcordApi as Api
import AmarcordApi.Common (Configuration (..), runWithConfiguration)
import AmarcordApi.Configuration (defaultConfiguration)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (Null))
import Data.Foldable (find)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Log.Backend.StandardOutput (withStdOutLogger)
import Log.Class (MonadLog, localDomain, logAttention_, logInfo_, logMessage)
import Log.Data (LogLevel (LogInfo), defaultLogLevel)
import Log.Monad (runLogT)
import Network.HTTP.Client (responseBody)
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import qualified Tango
import qualified TangoHL
import qualified UnliftIO

data CliOptions = CliOptions
  { cliBeamtimeId :: Int,
    cliDataSetId :: Int,
    cliIndexedFpsLowWatermark :: Float,
    cliTargetIndexedFrames :: Int,
    cliFramesPerRun :: Int,
    cliAmarcordUrl :: Text,
    cliP11RunnerIdentifier :: Text
  }

cliOptionsParser :: Opt.Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> Opt.option Opt.auto (Opt.long "beamtime-id")
    <*> Opt.option Opt.auto (Opt.long "data-set-id")
    <*> Opt.option Opt.auto (Opt.long "indexed-fps-low-watermark")
    <*> Opt.option Opt.auto (Opt.long "target-indexed-frames")
    <*> Opt.option Opt.auto (Opt.long "frames-per-run")
    <*> Opt.strOption (Opt.long "amarcord-url")
    <*> Opt.strOption (Opt.long "p11-runner-identifier")

data IndexedFrames = IndexedFrames
  { indexedFramesFrames :: Int,
    indexedFramesTime :: UTCTime
  }

retrieveIndexedFrames cliOptions = do
  result <-
    liftIO $
      runWithConfiguration
        (defaultConfiguration {configBaseURL = cliAmarcordUrl cliOptions})
        (Api.readAnalysisResultsApiAnalysisAnalysisResults_BeamtimeId_Get (cliBeamtimeId cliOptions))
  indexedFramesForRunAnalysis (cliDataSetId cliOptions) (responseBody result)

indexedFramesForRunAnalysis dataSetId result =
  case result of
    Api.ReadAnalysisResultsApiAnalysisAnalysisResultsBeamtimeIdGetResponse200 (Api.JsonReadAnalysisResults {Api.jsonReadAnalysisResultsDataSets = dataSets}) ->
      case find (\ds -> Api.jsonDataSetId (Api.jsonAnalysisDataSetDataSet ds) == dataSetId) (dataSets >>= Api.jsonAnalysisExperimentTypeDataSets) of
        Nothing -> pure $ Left "🔢 data set not found!"
        Just (Api.JsonAnalysisDataSet {Api.jsonAnalysisDataSetDataSet = (Api.JsonDataSet {Api.jsonDataSetSummary = summary})}) ->
          case summary of
            Nothing -> pure $ Left "🔢 data set has no indexing data!"
            Just (Api.JsonIndexingFom {Api.jsonIndexingFomIndexedFrames = indexedFrames}) -> do
              currentTime <- liftIO getCurrentTime
              pure $ Right (IndexedFrames indexedFrames currentTime)

startRun :: (UnliftIO.MonadUnliftIO m, MonadLog m) => CliOptions -> Tango.DeviceProxyPtr -> m ()
startRun cliOptions runner = do
  logInfo_ $ "📡 Setting number of images to " <> pack (show (cliFramesPerRun cliOptions))
  TangoHL.writeIntAttribute runner "number_of_images" (cliFramesPerRun cliOptions)
  logInfo_ "📡 Starting run"
  TangoHL.commandInOutVoid runner "start_run"

stopRun :: (UnliftIO.MonadUnliftIO m, MonadLog m) => Tango.DeviceProxyPtr -> m ()
stopRun runner = do
  logInfo_ "📡 Stopping run"
  TangoHL.commandInOutVoid runner "stop_run"

data RunnerStatus
  = Measuring
  | PreparedToMeasure
  | UnknownMoving
  | UnknownStatic
  | PreparedToOpenHutch
  | PreparingToOpenHutch
  | PreparingForMeasurement
  | StoppingChopper

instance Show RunnerStatus where
  show Measuring = "measuring"
  show PreparedToMeasure = "prepared to measure"
  show UnknownMoving = "moving"
  show UnknownStatic = "not moving"
  show PreparedToOpenHutch = "prepared to open hutch"
  show PreparingForMeasurement = "preparing for measurement"
  show StoppingChopper = "stopping chopper"

readRunnerStatus :: (UnliftIO.MonadUnliftIO m) => Tango.DeviceProxyPtr -> m RunnerStatus
readRunnerStatus runner = do
  statusCode <- TangoHL.readStringAttribute runner "state_enum"
  pure $ case statusCode of
    "measuring" -> Measuring
    "prepared_to_measure" -> PreparedToMeasure
    "unknown_moving" -> UnknownMoving
    "unknown_static" -> UnknownStatic
    "prepared_to_open_hutch" -> PreparedToOpenHutch
    "preparing_to_open_hutch" -> PreparingToOpenHutch
    "stopping_chopper" -> StoppingChopper

runningWindowSize = 5

calculateIndexedFps :: NE.NonEmpty IndexedFrames -> Maybe Float
calculateIndexedFps indexedFrames =
  if NE.length indexedFrames < runningWindowSize
    then Nothing
    else
      let first' = NE.head indexedFrames
          last' = NE.last indexedFrames
          timeDiff = nominalDiffTimeToSeconds (indexedFramesTime last' `diffUTCTime` indexedFramesTime first')
       in Just $ realToFrac (fromIntegral (indexedFramesFrames last' - indexedFramesFrames first')) / realToFrac timeDiff

consLimitedNe :: a -> NE.NonEmpty a -> NE.NonEmpty a
consLimitedNe newElement oldElements =
  if NE.length oldElements < runningWindowSize
    then NE.cons newElement oldElements
    else -- init: everything except the last
      newElement NE.:| NE.init oldElements

collectionLoop ::
  (UnliftIO.MonadUnliftIO m, MonadLog m) =>
  CliOptions ->
  Tango.DeviceProxyPtr ->
  NE.NonEmpty IndexedFrames ->
  m ()
collectionLoop cliOptions runner indexedFrameList = do
  logInfo_ "📡 Waiting for 30s"
  liftIO $ threadDelay (1000 * 1000 * 30)

  logInfo_ "📡 Let's see if we are still collecting..."

  statusCode <- readRunnerStatus runner
  let indexedFpsHighEnough = case calculateIndexedFps indexedFrameList of
        Just indexedFps -> indexedFps >= cliIndexedFpsLowWatermark cliOptions
        Nothing -> True

  validState <- case statusCode of
    Measuring ->
      if indexedFpsHighEnough
        then pure True
        else do
          logAttention_ "⚠ Oh no 🙁 We are still measuring, but the indexed frames per second is too low. Stopping the run and the main loop."
          stopRun runner
          pure False
    PreparedToMeasure ->
      if indexedFpsHighEnough
        then do
          logInfo_ "🔢 Run is over, and the indexed FPS looks good, starting another run"
          startRun cliOptions runner
          pure True
        else do
          logAttention_ "⚠ Oh no 🙁 We are at the end of the run, but the indexed frames per second is too low. Stopping the main loop."
          pure False
    PreparingForMeasurement -> pure True
    UnknownMoving -> pure True
    _otherState -> pure False

  when validState $ do
    do
      when indexedFpsHighEnough $ do
        logInfo_ "🔢 still measuring, waiting"
        indexedFramesResult <- retrieveIndexedFrames cliOptions
        case indexedFramesResult of
          Left e -> logAttention_ $ "⚠ error retrieving indexed frames: " <> e
          Right indF@(IndexedFrames {indexedFramesFrames}) -> do
            logInfo_ $ "🔢 got " <> pack (show indexedFramesFrames) <> " indexed frames for data set"
            collectionLoop cliOptions runner (consLimitedNe indF indexedFrameList)

withRunner :: (UnliftIO.MonadUnliftIO m, MonadLog m) => CliOptions -> Tango.DeviceProxyPtr -> m ()
withRunner cliOptions runner = do
  logInfo_ "📡 Connection to Tango runner established!"
  localDomain ("data set " <> pack (show (cliDataSetId cliOptions))) $ do
    logInfo_ "🔢 Let's see how many indexed frames we already have for this data set..."
    indexedFramesResult <- retrieveIndexedFrames cliOptions
    case indexedFramesResult of
      Left e -> logAttention_ $ "⚠ error retrieving data set: " <> e
      Right indF@(IndexedFrames {indexedFramesFrames}) -> do
        logInfo_ $ "🔢 got " <> pack (show indexedFramesFrames) <> " indexed frames for data set"
        if indexedFramesFrames > cliTargetIndexedFrames cliOptions
          then logInfo_ "🔢 Aha! We already have enough indexed frames. Please choose a higher target to collect more."
          else do
            logInfo_ "📡 Let's check the P11 runner's status..."
            statusCode <- readRunnerStatus runner
            logInfo_ $ "📡 Status is " <> pack (show statusCode)
            continue <- case statusCode of
              PreparedToMeasure -> do
                startRun cliOptions runner
                pure True
              Measuring -> do
                logInfo_ "📡 P11 runner is already measuring stuff. Cool. I'll wait then 👍"
                pure True
              otherCode -> do
                logInfo_ $
                  "📡 P11 runner is in status " <> pack (show otherCode) <> ", cannot proceed 👎"
                pure False
            when continue $ collectionLoop cliOptions runner (NE.singleton indF)

main :: IO ()
main = do
  let opts =
        Opt.info
          (cliOptionsParser <**> Opt.helper)
          ( Opt.fullDesc <> Opt.progDesc "Automate CFEL TapeDrive 2.0 data collection" <> Opt.header "chef - cook some crystal recipes!"
          )
  cliOptions <- Opt.execParser opts
  withStdOutLogger $ \logger ->
    runLogT
      "main"
      logger
      defaultLogLevel
      $ do
        logInfo_ "📡 Connecting to Tango P11 runner"
        TangoHL.withDeviceProxy
          (cliP11RunnerIdentifier cliOptions)
          (withRunner cliOptions)
