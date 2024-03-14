-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the types generated from the schema JsonReadExperimentTypes
module AmarcordApi.Types.JsonReadExperimentTypes where

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
import {-# SOURCE #-} AmarcordApi.Types.JsonAttributo
import {-# SOURCE #-} AmarcordApi.Types.JsonExperimentType
import {-# SOURCE #-} AmarcordApi.Types.JsonExperimentTypeAndRuns

-- | Defines the object schema located at @components.schemas.JsonReadExperimentTypes@ in the specification.
-- 
-- 
data JsonReadExperimentTypes = JsonReadExperimentTypes {
  -- | attributi
  jsonReadExperimentTypesAttributi :: ([JsonAttributo])
  -- | experiment_type_id_to_run
  , jsonReadExperimentTypesExperimentTypeIdToRun :: ([JsonExperimentTypeAndRuns])
  -- | experiment_types
  , jsonReadExperimentTypesExperimentTypes :: ([JsonExperimentType])
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON JsonReadExperimentTypes
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (["attributi" Data.Aeson.Types.ToJSON..= jsonReadExperimentTypesAttributi obj] : ["experiment_type_id_to_run" Data.Aeson.Types.ToJSON..= jsonReadExperimentTypesExperimentTypeIdToRun obj] : ["experiment_types" Data.Aeson.Types.ToJSON..= jsonReadExperimentTypesExperimentTypes obj] : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (["attributi" Data.Aeson.Types.ToJSON..= jsonReadExperimentTypesAttributi obj] : ["experiment_type_id_to_run" Data.Aeson.Types.ToJSON..= jsonReadExperimentTypesExperimentTypeIdToRun obj] : ["experiment_types" Data.Aeson.Types.ToJSON..= jsonReadExperimentTypesExperimentTypes obj] : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON JsonReadExperimentTypes
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "JsonReadExperimentTypes" (\obj -> ((GHC.Base.pure JsonReadExperimentTypes GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "attributi")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "experiment_type_id_to_run")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "experiment_types"))}
-- | Create a new 'JsonReadExperimentTypes' with all required fields.
mkJsonReadExperimentTypes :: [JsonAttributo] -- ^ 'jsonReadExperimentTypesAttributi'
  -> [JsonExperimentTypeAndRuns] -- ^ 'jsonReadExperimentTypesExperimentTypeIdToRun'
  -> [JsonExperimentType] -- ^ 'jsonReadExperimentTypesExperimentTypes'
  -> JsonReadExperimentTypes
mkJsonReadExperimentTypes jsonReadExperimentTypesAttributi jsonReadExperimentTypesExperimentTypeIdToRun jsonReadExperimentTypesExperimentTypes = JsonReadExperimentTypes{jsonReadExperimentTypesAttributi = jsonReadExperimentTypesAttributi,
                                                                                                                                                                         jsonReadExperimentTypesExperimentTypeIdToRun = jsonReadExperimentTypesExperimentTypeIdToRun,
                                                                                                                                                                         jsonReadExperimentTypesExperimentTypes = jsonReadExperimentTypesExperimentTypes}