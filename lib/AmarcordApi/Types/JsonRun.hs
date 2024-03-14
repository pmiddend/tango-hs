-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the types generated from the schema JsonRun
module AmarcordApi.Types.JsonRun where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Control.Monad.Fail
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Encoding.Internal
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.ByteString
import qualified Data.ByteString as Data.ByteString.Internal
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text as Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified AmarcordApi.Common
import AmarcordApi.TypeAlias
import {-# SOURCE #-} AmarcordApi.Types.JsonAttributoValue
import {-# SOURCE #-} AmarcordApi.Types.JsonFileOutput
import {-# SOURCE #-} AmarcordApi.Types.JsonIndexingFom

-- | Defines the object schema located at @components.schemas.JsonRun@ in the specification.
-- 
-- 
data JsonRun = JsonRun {
  -- | attributi
  jsonRunAttributi :: ([JsonAttributoValue])
  -- | data_sets
  , jsonRunDataSets :: ([GHC.Types.Int])
  -- | experiment_type_id
  , jsonRunExperimentTypeId :: GHC.Types.Int
  -- | external_id
  , jsonRunExternalId :: GHC.Types.Int
  -- | files
  , jsonRunFiles :: ([JsonFileOutput])
  -- | id
  , jsonRunId :: GHC.Types.Int
  -- | running_indexing_jobs
  , jsonRunRunningIndexingJobs :: ([GHC.Types.Int])
  -- | started
  , jsonRunStarted :: GHC.Types.Int
  -- | stopped
  , jsonRunStopped :: (GHC.Maybe.Maybe GHC.Types.Int)
  -- | summary
  , jsonRunSummary :: JsonIndexingFom
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON JsonRun
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (["attributi" Data.Aeson.Types.ToJSON..= jsonRunAttributi obj] : ["data_sets" Data.Aeson.Types.ToJSON..= jsonRunDataSets obj] : ["experiment_type_id" Data.Aeson.Types.ToJSON..= jsonRunExperimentTypeId obj] : ["external_id" Data.Aeson.Types.ToJSON..= jsonRunExternalId obj] : ["files" Data.Aeson.Types.ToJSON..= jsonRunFiles obj] : ["id" Data.Aeson.Types.ToJSON..= jsonRunId obj] : ["running_indexing_jobs" Data.Aeson.Types.ToJSON..= jsonRunRunningIndexingJobs obj] : ["started" Data.Aeson.Types.ToJSON..= jsonRunStarted obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("stopped" Data.Aeson.Types.ToJSON..=)) (jsonRunStopped obj) : ["summary" Data.Aeson.Types.ToJSON..= jsonRunSummary obj] : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (["attributi" Data.Aeson.Types.ToJSON..= jsonRunAttributi obj] : ["data_sets" Data.Aeson.Types.ToJSON..= jsonRunDataSets obj] : ["experiment_type_id" Data.Aeson.Types.ToJSON..= jsonRunExperimentTypeId obj] : ["external_id" Data.Aeson.Types.ToJSON..= jsonRunExternalId obj] : ["files" Data.Aeson.Types.ToJSON..= jsonRunFiles obj] : ["id" Data.Aeson.Types.ToJSON..= jsonRunId obj] : ["running_indexing_jobs" Data.Aeson.Types.ToJSON..= jsonRunRunningIndexingJobs obj] : ["started" Data.Aeson.Types.ToJSON..= jsonRunStarted obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("stopped" Data.Aeson.Types.ToJSON..=)) (jsonRunStopped obj) : ["summary" Data.Aeson.Types.ToJSON..= jsonRunSummary obj] : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON JsonRun
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "JsonRun" (\obj -> (((((((((GHC.Base.pure JsonRun GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "attributi")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "data_sets")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "experiment_type_id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "external_id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "files")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "running_indexing_jobs")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "started")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "stopped")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "summary"))}
-- | Create a new 'JsonRun' with all required fields.
mkJsonRun :: [JsonAttributoValue] -- ^ 'jsonRunAttributi'
  -> [GHC.Types.Int] -- ^ 'jsonRunDataSets'
  -> GHC.Types.Int -- ^ 'jsonRunExperimentTypeId'
  -> GHC.Types.Int -- ^ 'jsonRunExternalId'
  -> [JsonFileOutput] -- ^ 'jsonRunFiles'
  -> GHC.Types.Int -- ^ 'jsonRunId'
  -> [GHC.Types.Int] -- ^ 'jsonRunRunningIndexingJobs'
  -> GHC.Types.Int -- ^ 'jsonRunStarted'
  -> JsonIndexingFom -- ^ 'jsonRunSummary'
  -> JsonRun
mkJsonRun jsonRunAttributi jsonRunDataSets jsonRunExperimentTypeId jsonRunExternalId jsonRunFiles jsonRunId jsonRunRunningIndexingJobs jsonRunStarted jsonRunSummary = JsonRun{jsonRunAttributi = jsonRunAttributi,
                                                                                                                                                                               jsonRunDataSets = jsonRunDataSets,
                                                                                                                                                                               jsonRunExperimentTypeId = jsonRunExperimentTypeId,
                                                                                                                                                                               jsonRunExternalId = jsonRunExternalId,
                                                                                                                                                                               jsonRunFiles = jsonRunFiles,
                                                                                                                                                                               jsonRunId = jsonRunId,
                                                                                                                                                                               jsonRunRunningIndexingJobs = jsonRunRunningIndexingJobs,
                                                                                                                                                                               jsonRunStarted = jsonRunStarted,
                                                                                                                                                                               jsonRunStopped = GHC.Maybe.Nothing,
                                                                                                                                                                               jsonRunSummary = jsonRunSummary}