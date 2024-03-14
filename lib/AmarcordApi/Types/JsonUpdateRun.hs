-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the types generated from the schema JsonUpdateRun
module AmarcordApi.Types.JsonUpdateRun where

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

-- | Defines the object schema located at @components.schemas.JsonUpdateRun@ in the specification.
-- 
-- 
data JsonUpdateRun = JsonUpdateRun {
  -- | attributi
  jsonUpdateRunAttributi :: ([JsonAttributoValue])
  -- | beamtime_id
  , jsonUpdateRunBeamtimeId :: GHC.Types.Int
  -- | experiment_type_id
  , jsonUpdateRunExperimentTypeId :: GHC.Types.Int
  -- | id
  , jsonUpdateRunId :: GHC.Types.Int
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON JsonUpdateRun
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (["attributi" Data.Aeson.Types.ToJSON..= jsonUpdateRunAttributi obj] : ["beamtime_id" Data.Aeson.Types.ToJSON..= jsonUpdateRunBeamtimeId obj] : ["experiment_type_id" Data.Aeson.Types.ToJSON..= jsonUpdateRunExperimentTypeId obj] : ["id" Data.Aeson.Types.ToJSON..= jsonUpdateRunId obj] : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (["attributi" Data.Aeson.Types.ToJSON..= jsonUpdateRunAttributi obj] : ["beamtime_id" Data.Aeson.Types.ToJSON..= jsonUpdateRunBeamtimeId obj] : ["experiment_type_id" Data.Aeson.Types.ToJSON..= jsonUpdateRunExperimentTypeId obj] : ["id" Data.Aeson.Types.ToJSON..= jsonUpdateRunId obj] : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON JsonUpdateRun
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "JsonUpdateRun" (\obj -> (((GHC.Base.pure JsonUpdateRun GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "attributi")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "beamtime_id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "experiment_type_id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "id"))}
-- | Create a new 'JsonUpdateRun' with all required fields.
mkJsonUpdateRun :: [JsonAttributoValue] -- ^ 'jsonUpdateRunAttributi'
  -> GHC.Types.Int -- ^ 'jsonUpdateRunBeamtimeId'
  -> GHC.Types.Int -- ^ 'jsonUpdateRunExperimentTypeId'
  -> GHC.Types.Int -- ^ 'jsonUpdateRunId'
  -> JsonUpdateRun
mkJsonUpdateRun jsonUpdateRunAttributi jsonUpdateRunBeamtimeId jsonUpdateRunExperimentTypeId jsonUpdateRunId = JsonUpdateRun{jsonUpdateRunAttributi = jsonUpdateRunAttributi,
                                                                                                                             jsonUpdateRunBeamtimeId = jsonUpdateRunBeamtimeId,
                                                                                                                             jsonUpdateRunExperimentTypeId = jsonUpdateRunExperimentTypeId,
                                                                                                                             jsonUpdateRunId = jsonUpdateRunId}