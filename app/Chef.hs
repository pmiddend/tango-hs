{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified AmarcordApi as Api
import AmarcordApi.Common (Configuration (..), runWithConfiguration)
import AmarcordApi.Configuration (defaultConfiguration)
import Control.Concurrent (ThreadId, forkFinally, killThread, threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity)
import Data.Foldable (find, for_)
import Data.IORef (IORef, newIORef)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Ratio ((%))
import Data.String (IsString)
import Data.Text (Text, intercalate, pack, unpack)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy (def, layout_title, line, plot, plotLeft, plotRight, (.=))
import Log.Backend.StandardOutput (withStdOutLogger)
import Log.Class (MonadLog, localDomain, logAttention_, logInfo_)
import Log.Data (defaultLogLevel)
import Log.Monad (runLogT)
import Lucid (renderText)
import qualified Lucid as L
import qualified Lucid.Htmx as LX
import Network.HTTP.Client (responseBody)
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import qualified Tango
import qualified TangoHL
import qualified UnliftIO
import Web.Scotty (file, get, html, param, post, scotty, setHeader, text)

data CollectionConfig = CollectionConfig
  { configIndexedFpsLowWatermark :: Float,
    configTargetIndexedFrames :: Int,
    configFramesPerRun :: Int,
    configDataSetId :: Int
  }
  deriving (Show)

data CliOptions = CliOptions
  { cliBeamtimeId :: Int,
    cliWebPort :: Int,
    cliAmarcordUrl :: Text,
    cliP11RunnerIdentifier :: Text
  }

cliOptionsParser :: Opt.Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> Opt.option Opt.auto (Opt.long "beamtime-id")
    <*> Opt.option Opt.auto (Opt.long "web-port" <> Opt.value 3000 <> Opt.showDefault)
    <*> Opt.strOption (Opt.long "amarcord-url")
    <*> Opt.strOption (Opt.long "p11-runner-identifier")

data IndexedFrames = IndexedFrames
  { indexedFramesFrames :: !Int,
    indexedFramesTime :: !UTCTime
  }
  deriving (Show)

atomicWriteIORefVoid :: (MonadIO m) => IORef a -> a -> m ()
atomicWriteIORefVoid ref v = UnliftIO.atomicModifyIORef ref (const (v, ()))

runAmarcordHttp :: (MonadIO m) => Text -> Api.ClientT IO a -> m a
runAmarcordHttp amarcordUrl request = liftIO (runWithConfiguration (defaultConfiguration {configBaseURL = amarcordUrl}) request)

retrieveIndexedFrames :: (MonadIO m) => CliOptions -> Int -> m (Either Text IndexedFrames)
retrieveIndexedFrames cliOptions dataSetId = do
  result <- runAmarcordHttp (cliAmarcordUrl cliOptions) (Api.readAnalysisResultsApiAnalysisAnalysisResults_BeamtimeId_Get (cliBeamtimeId cliOptions))
  indexedFramesForRunAnalysis dataSetId (responseBody result)

indexedFramesForRunAnalysis :: (MonadIO f) => Int -> Api.ReadAnalysisResultsApiAnalysisAnalysisResultsBeamtimeIdGetResponse -> f (Either Text IndexedFrames)
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
    _ -> pure $ Left "invalid HTTP response (FIXME: add more error info here)"

startRun :: (UnliftIO.MonadUnliftIO m, MonadLog m) => LoopStateRef -> Int -> Tango.DeviceProxyPtr -> m ()
startRun loopStateRef framesPerRun runner = do
  logInfo loopStateRef $ "📡 Setting number of images to " <> pack (show framesPerRun)
  TangoHL.writeIntAttribute runner "number_of_images" framesPerRun
  logInfo loopStateRef "📡 Starting run"
  TangoHL.commandInOutVoid runner "start_run"

stopRun :: (UnliftIO.MonadUnliftIO m, MonadLog m) => LoopStateRef -> Tango.DeviceProxyPtr -> m ()
stopRun loopStateRef runner = do
  logInfo loopStateRef "📡 Stopping run"
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
  show PreparingToOpenHutch = "preparing to open hutch"

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
    invalidStatusCode -> error $ "invalid status code " <> unpack invalidStatusCode

runningWindowSize :: Int
runningWindowSize = 12

peekTimeSeconds :: Int
peekTimeSeconds = 5

calculateIndexedFpsInWindow :: NE.NonEmpty IndexedFrames -> Maybe Float
calculateIndexedFpsInWindow indexedFrames =
  if NE.length indexedFrames < runningWindowSize
    then Nothing
    else calculateIndexedFps indexedFrames

calculateIndexedFps :: NE.NonEmpty IndexedFrames -> Maybe Float
calculateIndexedFps indexedFrames =
  let first' = NE.head indexedFrames
      last' = NE.last indexedFrames
      timeDiff = nominalDiffTimeToSeconds (indexedFramesTime last' `diffUTCTime` indexedFramesTime first')
   in Just $ realToFrac @Integer @Float (fromIntegral (indexedFramesFrames last' - indexedFramesFrames first')) / realToFrac timeDiff

consLimitedNe :: a -> NE.NonEmpty a -> NE.NonEmpty a
consLimitedNe newElement oldElements =
  if NE.length oldElements < runningWindowSize
    then NE.cons newElement oldElements
    else -- init: everything except the last
      newElement NE.:| NE.init oldElements

data LogMessage = LogMessage
  { logTime :: UTCTime,
    logMessage :: Text
  }
  deriving (Show)

data CollectionLoopState = CollectionLoopState
  { loopStateIndexedFrameHistory :: ![IndexedFrames],
    loopStateRunIds :: ![(UTCTime, Int)],
    loopStateCollectionConfig :: CollectionConfig,
    loopStateLog :: [LogMessage]
  }
  deriving (Show)

type LoopStateRef = IORef (Maybe CollectionLoopState)

logInfo :: (MonadLog m, MonadIO m) => IORef (Maybe CollectionLoopState) -> Text -> m ()
logInfo loopStateRef message = do
  logInfo_ message
  currentTime <- liftIO getCurrentTime
  atomicModifyIORefVoid
    loopStateRef
    (\old -> (\state -> state {loopStateLog = LogMessage currentTime message : loopStateLog state}) <$> old)

collectionLoop :: CliOptions -> CollectionConfig -> LoopStateRef -> IO ()
collectionLoop cliOptions collectionConfig loopStateRef = withStdOutLogger $ \logger -> do
  atomicWriteIORefVoid
    loopStateRef
    ( Just
        ( CollectionLoopState
            { loopStateIndexedFrameHistory = [],
              loopStateRunIds = [],
              loopStateCollectionConfig = collectionConfig,
              loopStateLog = []
            }
        )
    )
  runLogT
    "main"
    logger
    defaultLogLevel
    $ do
      logInfo loopStateRef "📡 Connecting to Tango P11 runner"
      TangoHL.withDeviceProxy
        (cliP11RunnerIdentifier cliOptions)
        (withRunner cliOptions collectionConfig loopStateRef)

appendIndexedFrames :: (MonadIO m) => IORef (Maybe CollectionLoopState) -> IndexedFrames -> m ()
appendIndexedFrames loopStateRef indF =
  atomicModifyIORefVoid
    loopStateRef
    (\maybeCurrentState -> (\currentState -> currentState {loopStateIndexedFrameHistory = indF : loopStateIndexedFrameHistory currentState}) <$> maybeCurrentState)

appendRunId :: (MonadIO m) => IORef (Maybe CollectionLoopState) -> Int -> m ()
appendRunId loopStateRef newRunId = do
  currentTime <- liftIO getCurrentTime
  atomicModifyIORefVoid
    loopStateRef
    (\maybeCurrentState -> (\currentState -> currentState {loopStateRunIds = (currentTime, newRunId) : loopStateRunIds currentState}) <$> maybeCurrentState)

collectionLoop' ::
  (UnliftIO.MonadUnliftIO m, MonadLog m) =>
  CliOptions ->
  CollectionConfig ->
  LoopStateRef ->
  Tango.DeviceProxyPtr ->
  NE.NonEmpty IndexedFrames ->
  m ()
collectionLoop' cliOptions collectionConfig loopStateRef runner indexedFrameList = do
  logInfo loopStateRef $ "📡 Waiting for " <> pack (show peekTimeSeconds) <> "s"
  liftIO $ threadDelay (1000 * 1000 * peekTimeSeconds)

  logInfo loopStateRef "📡 Let's see if we are still collecting..."

  statusCode <- readRunnerStatus runner
  let indexedFpsHighEnough = case calculateIndexedFpsInWindow indexedFrameList of
        Just indexedFps -> indexedFps >= configIndexedFpsLowWatermark collectionConfig
        Nothing -> True

  validState <- case statusCode of
    Measuring ->
      if indexedFpsHighEnough
        then pure True
        else do
          logAttention_ "⚠ Oh no 🙁 We are still measuring, but the indexed frames per second is too low. Stopping the run and the main loop."
          stopRun loopStateRef runner
          pure False
    PreparedToMeasure ->
      if indexedFpsHighEnough
        then do
          logInfo loopStateRef "🔢 Run is over, and the indexed FPS looks good, starting another run"
          startRun loopStateRef (configFramesPerRun collectionConfig) runner
          runId <- TangoHL.readIntAttribute runner "run_id"
          appendRunId loopStateRef runId
          pure True
        else
          if indexedFramesFrames (NE.head indexedFrameList) > configTargetIndexedFrames collectionConfig
            then do
              logAttention_ "🎉 We have arrived at the desired number of frames, stopping!"
              pure False
            else do
              logAttention_ "⚠ Oh no 🙁 We are at the end of the run, but the indexed frames per second is too low. Stopping the main loop."
              pure False
    PreparingForMeasurement -> pure True
    UnknownMoving -> pure True
    _otherState -> pure False

  when validState $ do
    do
      when indexedFpsHighEnough $ do
        logInfo loopStateRef "🔢 still measuring, waiting"
        indexedFramesResult <- retrieveIndexedFrames cliOptions (configDataSetId collectionConfig)
        case indexedFramesResult of
          Left e -> logAttention_ $ "⚠ error retrieving indexed frames: " <> e
          Right indF@(IndexedFrames {indexedFramesFrames}) -> do
            logInfo loopStateRef $ "🔢 got " <> pack (show indexedFramesFrames) <> " indexed frames for data set"
            appendIndexedFrames loopStateRef indF
            collectionLoop' cliOptions collectionConfig loopStateRef runner (consLimitedNe indF indexedFrameList)

atomicModifyIORefVoid :: (MonadIO m) => IORef t -> (t -> t) -> m ()
atomicModifyIORefVoid ref f = UnliftIO.atomicModifyIORef ref (\x -> (f x, ()))

withRunner :: (UnliftIO.MonadUnliftIO m, MonadLog m) => CliOptions -> CollectionConfig -> LoopStateRef -> Tango.DeviceProxyPtr -> m ()
withRunner cliOptions collectionConfig loopStateRef runner = do
  logInfo loopStateRef "📡 Connection to Tango runner established!"
  localDomain ("data set " <> pack (show (configDataSetId collectionConfig))) $ do
    logInfo loopStateRef "🔢 Let's see how many indexed frames we already have for this data set..."
    indexedFramesResult <- retrieveIndexedFrames cliOptions (configDataSetId collectionConfig)
    case indexedFramesResult of
      Left e -> logAttention_ $ "⚠ error retrieving data set: " <> e
      Right indF@(IndexedFrames {indexedFramesFrames}) -> do
        appendIndexedFrames loopStateRef indF
        logInfo loopStateRef $ "🔢 got " <> pack (show indexedFramesFrames) <> " indexed frames for data set"
        if indexedFramesFrames > configTargetIndexedFrames collectionConfig
          then logInfo loopStateRef "🔢 Aha! We already have enough indexed frames. Please choose a higher target to collect more."
          else do
            runId <- TangoHL.readIntAttribute runner "run_id"
            appendRunId loopStateRef runId
            logInfo loopStateRef $ "📡 Latest run ID is " <> pack (show runId) <> "..."
            logInfo loopStateRef "📡 Let's check the P11 runner's status..."
            statusCode <- readRunnerStatus runner
            logInfo loopStateRef $ "📡 Status is " <> pack (show statusCode)
            continue <- case statusCode of
              PreparedToMeasure -> do
                startRun loopStateRef (configFramesPerRun collectionConfig) runner
                pure True
              Measuring -> do
                logInfo loopStateRef "📡 P11 runner is already measuring stuff. Cool. I'll wait then 👍"
                pure True
              otherCode -> do
                logInfo loopStateRef $
                  "📡 P11 runner is in status " <> pack (show otherCode) <> ", cannot proceed 👎"
                pure False
            when continue $ collectionLoop' cliOptions collectionConfig loopStateRef runner (NE.singleton indF)

dataSetIdParam :: (IsString a) => a
dataSetIdParam = "dataSetId"

indexedFpsLowWatermarkParam :: (IsString a) => a
indexedFpsLowWatermarkParam = "indexedFpsLowWatermark"

targetIndexedFramesParam :: (IsString a) => a
targetIndexedFramesParam = "targetIndexedFrames"

framesPerRunParam :: (IsString a) => a
framesPerRunParam = "framesPerRun"

currentStateHtml :: UTCTime -> TimeZone -> Maybe CollectionLoopState -> L.HtmlT Identity ()
currentStateHtml currentTime tz currentState =
  case currentState of
    Nothing -> L.div_ [L.class_ "form-text"] (L.p_ "Please select a data set and start collecting!")
    Just (CollectionLoopState {loopStateLog = loglines}) ->
      L.div_ [L.class_ "row"] $
        do
          L.div_ [L.class_ "col-6"] $ do
            L.h3_ "Stats"
            L.p_ $ do
              case currentState >>= NE.nonEmpty . take runningWindowSize . loopStateIndexedFrameHistory of
                Nothing -> mempty
                Just frameHistoryNonEmpty ->
                  L.strong_ (L.toHtml $ "FPS: " <> pack (show (calculateIndexedFps frameHistoryNonEmpty)))
            L.h3_ "Log"
            L.ul_ (mapM_ (\(LogMessage {logTime = t, logMessage = m}) -> L.li_ (L.span_ [L.class_ "text-muted"] (L.toHtml $ formatTime defaultTimeLocale "%T" (utcToLocalTime tz t)) <> " " <> L.toHtml m)) loglines)
          L.div_ [L.class_ "col-6"] $ do
            L.img_
              [ L.src_ ("/chart?time=" <> pack (show (utcTimeToPOSIXSeconds currentTime))),
                L.style_ "width: 100%"
              ]

htmlSkeleton :: (Monad m) => L.HtmlT m a -> L.HtmlT m a
htmlSkeleton content = do
  L.doctypehtml_ $ do
    L.head_ $ do
      LX.useHtmx
      L.meta_ [L.charset_ "utf-8"]
      L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
      L.title_ "chef - cook some crystal recipes!"
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css", L.rel_ "stylesheet"]
    L.body_ $ do
      L.div_ [L.class_ "container"] $ do
        L.h1_ "chef - cook some crystal recipes!"
        content

data TypedAttributoValue
  = TypedAttributoValueChemical Api.JsonChemical
  | TypedAttributoValueDateTime UTCTime
  | TypedAttributoValueInt Int
  | TypedAttributoValueString Text (Maybe [Text])
  | TypedAttributoValueBoolean Bool
  | TypedAttributoValueNumber Double Api.JSONSchemaNumber

createTypedAttributoValue :: Map.Map Int Api.JsonChemical -> Api.JsonAttributoValue -> Api.JsonAttributo -> Either Text TypedAttributoValue
createTypedAttributoValue
  chemicalIdToType
  (Api.JsonAttributoValue {Api.jsonAttributoValueAttributoValueInt = Just chemicalId})
  ( Api.JsonAttributo
      { Api.jsonAttributoAttributoTypeInteger = Just (Api.JSONSchemaInteger {Api.jSONSchemaIntegerFormat = Just Api.JSONSchemaIntegerFormat'EnumChemicalId})
      }
    ) = case Map.lookup chemicalId chemicalIdToType of
    Nothing -> Left $ "unknown chemical id " <> pack (show chemicalId)
    Just chemical -> Right $ TypedAttributoValueChemical chemical
createTypedAttributoValue
  _
  (Api.JsonAttributoValue {Api.jsonAttributoValueAttributoValueInt = Just dateTimeValue})
  ( Api.JsonAttributo
      { Api.jsonAttributoAttributoTypeInteger =
          Just (Api.JSONSchemaInteger {Api.jSONSchemaIntegerFormat = Just Api.JSONSchemaIntegerFormat'EnumDateTime})
      }
    ) = Right $ TypedAttributoValueDateTime $ posixSecondsToUTCTime $ realToFrac $ dateTimeValue % 1000
createTypedAttributoValue
  _
  (Api.JsonAttributoValue {Api.jsonAttributoValueAttributoValueInt = Just intValue})
  _ = Right $ TypedAttributoValueInt intValue
createTypedAttributoValue
  _
  (Api.JsonAttributoValue {Api.jsonAttributoValueAttributoValueStr = Just stringValue})
  ( Api.JsonAttributo
      { Api.jsonAttributoAttributoTypeString =
          Just (Api.JSONSchemaString {Api.jSONSchemaStringEnum = enumValues})
      }
    ) = Right $ TypedAttributoValueString stringValue enumValues
createTypedAttributoValue
  _
  (Api.JsonAttributoValue {Api.jsonAttributoValueAttributoValueBool = Just boolValue})
  _ = Right $ TypedAttributoValueBoolean boolValue
createTypedAttributoValue
  _
  (Api.JsonAttributoValue {Api.jsonAttributoValueAttributoValueFloat = Just numberValue})
  ( Api.JsonAttributo
      { Api.jsonAttributoAttributoTypeNumber =
          Just numberSchema
      }
    ) = Right $ TypedAttributoValueNumber numberValue numberSchema
createTypedAttributoValue
  _
  value
  type_ = Left $ "invalid combination of attributo value and type\n\nvalue is " <> pack (show value) <> "\n\ntype is: " <> pack (show type_)

attributoValueAndTypeToText :: Map.Map Int Api.JsonChemical -> Api.JsonAttributoValue -> Api.JsonAttributo -> Text
attributoValueAndTypeToText chemicalIdToType value type_ =
  Api.jsonAttributoName type_ <> " " <> case createTypedAttributoValue chemicalIdToType value type_ of
    Right (TypedAttributoValueChemical c) -> Api.jsonChemicalName c
    Right (TypedAttributoValueDateTime dt) -> pack (iso8601Show dt)
    Right (TypedAttributoValueInt i) -> pack (show i)
    Right (TypedAttributoValueString t _enumValues) -> t
    Right (TypedAttributoValueBoolean b) -> pack (show b)
    Right (TypedAttributoValueNumber n _numberSchema) -> pack (show n)
    Left e -> "error: " <> e

stopForm :: (Monad m) => L.HtmlT m ()
stopForm =
  L.form_ [LX.hxPost_ "/stop"] $ do
    L.button_ [L.class_ "btn btn-warning mb-3", L.type_ "submit"] "Stop collection"

startForm :: (Monad m) => Maybe CollectionLoopState -> Api.JsonReadDataSets -> L.HtmlT m ()
startForm currentState dataSets =
  let attributoIdToType :: Map.Map Int Api.JsonAttributo
      attributoIdToType = Map.fromList ((\a -> (Api.jsonAttributoId a, a)) <$> Api.jsonReadDataSetsAttributi dataSets)
      chemicalIdToType :: Map.Map Int Api.JsonChemical
      chemicalIdToType = Map.fromList ((\c -> (Api.jsonChemicalId c, c)) <$> Api.jsonReadDataSetsChemicals dataSets)
      attributoValueToText :: Api.JsonAttributoValue -> Text
      attributoValueToText av@(Api.JsonAttributoValue {Api.jsonAttributoValueAttributoId = aid}) =
        maybe
          "unknown data set "
          (attributoValueAndTypeToText chemicalIdToType av)
          (Map.lookup aid attributoIdToType)
      makeOption :: (Monad m) => Api.JsonDataSet -> L.HtmlT m ()
      makeOption (Api.JsonDataSet {Api.jsonDataSetId = dataSetId, Api.jsonDataSetAttributi = attributi}) =
        L.option_
          ([L.value_ (pack (show dataSetId))] <> [L.disabled_ "true" | Just dataSetId == (configDataSetId . loopStateCollectionConfig <$> currentState)])
          (L.toHtml $ pack (show dataSetId) <> ": " <> intercalate ", " (attributoValueToText <$> attributi))
   in L.form_ [LX.hxPost_ "/start"] $ do
        L.div_ [L.class_ "mb-3"] $
          L.div_ [L.class_ "form-floating"] $ do
            L.select_
              ( [ L.class_ "form-select",
                  L.name_ dataSetIdParam,
                  L.id_ "dataSetIdSelect"
                ]
                  <> [L.disabled_ "true" | isJust currentState]
              )
              (mapM_ makeOption (Api.jsonReadDataSetsDataSets dataSets))
            L.label_ [L.for_ "dataSetIdSelect"] "Data Set"
        L.div_ [L.class_ "row mb-3"] $ do
          L.div_ [L.class_ "col-6"] $ do
            L.div_ [L.class_ "form-floating"] $ do
              L.input_
                [ L.type_ "number",
                  L.name_ targetIndexedFramesParam,
                  L.value_ "10000",
                  L.min_ "1",
                  L.step_ "1",
                  L.id_ targetIndexedFramesParam,
                  L.class_ "form-control"
                ]
              L.label_ [L.for_ targetIndexedFramesParam] "Target Indexed Frames"
          L.div_ [L.class_ "col-6"] $ do
            L.div_ [L.class_ "form-floating"] $ do
              L.input_
                [ L.type_ "number",
                  L.name_ indexedFpsLowWatermarkParam,
                  L.value_ "2",
                  L.min_ "0",
                  L.step_ "0.01",
                  L.id_ indexedFpsLowWatermarkParam,
                  L.class_ "form-control"
                ]
              L.label_ [L.for_ targetIndexedFramesParam] "Indexing Low Watermark"
              L.div_ [L.class_ "form-text"] "If collection falls below this value, expressed as indexed frames per second, it will be terminated."
        L.div_ [L.class_ "row mb-3"] $ do
          L.div_ [L.class_ "col-auto"] $ do
            L.div_ [L.class_ "form-floating"] $ do
              L.input_
                [ L.type_ "number",
                  L.name_ framesPerRunParam,
                  L.value_ "100",
                  L.min_ "1",
                  L.step_ "1",
                  L.id_ framesPerRunParam,
                  L.class_ "form-control"
                ]
              L.label_ [L.for_ framesPerRunParam] "Frames per Run"
        L.button_ [L.type_ "submit", L.class_ "btn btn-primary"] "Start collection"

runServer :: CliOptions -> IORef (Maybe CollectionLoopState) -> IORef (Maybe ThreadId) -> Api.JsonBeamtime -> Api.JsonReadDataSets -> IO ()
runServer cliOptions currentLoopState currentThread beamtimeMetadata dataSets = scotty (cliWebPort cliOptions) $ do
  get "/chart" $ do
    currentState <- UnliftIO.readIORef currentLoopState
    case currentState of
      Nothing -> text ""
      Just (CollectionLoopState {loopStateIndexedFrameHistory = frameHistory, loopStateRunIds = runIds}) ->
        case NE.nonEmpty frameHistory of
          Nothing -> text ""
          Just nonEmptyHistory -> do
            let historyOldToNew = NE.reverse nonEmptyHistory
                oldestTime = indexedFramesTime (NE.head historyOldToNew)
                runIdPoints :: [(Float, Int)]
                runIdPoints =
                  [ ( realToFrac @NominalDiffTime @Float (time `diffUTCTime` oldestTime),
                      runId
                    )
                    | (time, runId) <- runIds
                  ]
                fpsPoints :: [(Float, Int)]
                fpsPoints =
                  [ ( realToFrac @NominalDiffTime @Float (time `diffUTCTime` oldestTime),
                      fromIntegral frames
                    )
                    | IndexedFrames frames time <- NE.toList historyOldToNew
                  ]
            liftIO $ toFile def "chart.svg" $ do
              -- layout_title .= "Indexing statistics"
              plotLeft (line "Indexed frames" [fpsPoints])
              plotRight (line "Run IDs" [runIdPoints])
            setHeader "Content-Type" "image/svg+xml"
            file "chart.svg"
  post "/stop" $ do
    liftIO $ TangoHL.withDeviceProxy (cliP11RunnerIdentifier cliOptions) $ \runner -> TangoHL.commandInOutVoid runner "stop_run"
    currentThreadId <- UnliftIO.readIORef currentThread
    liftIO $ for_ currentThreadId killThread
    currentState <- UnliftIO.readIORef currentLoopState
    html $ renderText $ startForm currentState dataSets
  post "/start" $ do
    collectionConfig <-
      CollectionConfig
        <$> param indexedFpsLowWatermarkParam
        <*> param targetIndexedFramesParam
        <*> param framesPerRunParam
        <*> param dataSetIdParam
    -- FIXME: use an mvar to synchronize thread creation
    let doneHandler _eitherExceptionOrValue = do
          atomicWriteIORefVoid currentLoopState Nothing
          atomicWriteIORefVoid currentThread Nothing
    newThreadId <- liftIO $ forkFinally (collectionLoop cliOptions collectionConfig currentLoopState) doneHandler
    atomicWriteIORefVoid currentThread (Just newThreadId)
    html $ renderText $ L.div_ [] stopForm
  get "/current-state" $ do
    currentState <- UnliftIO.readIORef currentLoopState
    tz <- liftIO getCurrentTimeZone
    currentTime <- liftIO getCurrentTime
    html $ renderText $ currentStateHtml currentTime tz currentState
  get "/form-state" $ do
    currentState <- UnliftIO.readIORef currentLoopState
    html $ renderText $ case currentState of
      Nothing -> startForm currentState dataSets
      Just _ -> stopForm
  get "/" $ do
    currentState <- UnliftIO.readIORef currentLoopState
    currentTime <- liftIO getCurrentTime
    tz <- liftIO getCurrentTimeZone
    html $
      renderText $
        htmlSkeleton $ do
          L.h2_ $ L.toHtml $ Api.jsonBeamtimeTitle beamtimeMetadata
          case currentState of
            Nothing -> startForm currentState dataSets
            Just _ -> L.div_ [LX.hxGet_ "/form-state", LX.hxTrigger_ "every 1s"] stopForm
          L.hr_ []
          L.div_ [LX.hxTrigger_ "every 10s", LX.hxGet_ "/current-state"] $ do
            currentStateHtml currentTime tz currentState

main :: IO ()
main = do
  let opts =
        Opt.info
          (cliOptionsParser <**> Opt.helper)
          ( Opt.fullDesc
              <> Opt.progDesc "Automate CFEL TapeDrive 2.0 data collection"
              <> Opt.header "chef - cook some crystal recipes!"
          )
  cliOptions <- Opt.execParser opts
  currentLoopState <- newIORef Nothing
  beamtimeMetadata' <-
    runAmarcordHttp
      (cliAmarcordUrl cliOptions)
      (Api.readBeamtimeApiBeamtimes_BeamtimeId_Get (cliBeamtimeId cliOptions))
  case responseBody beamtimeMetadata' of
    Api.ReadBeamtimeApiBeamtimesBeamtimeIdGetResponse200 beamtimeMetadata -> do
      dataSets' <- runAmarcordHttp (cliAmarcordUrl cliOptions) (Api.readDataSetsApiDataSets_BeamtimeId_Get (cliBeamtimeId cliOptions))
      case responseBody dataSets' of
        Api.ReadDataSetsApiDataSetsBeamtimeIdGetResponse200 dataSets -> do
          currentThread <- newIORef Nothing
          runServer cliOptions currentLoopState currentThread beamtimeMetadata dataSets
        someOtherError -> putStrLn $ "error getting data sets: " <> show someOtherError
    someOtherError -> putStrLn $ "error getting beam time metadata: " <> show someOtherError
