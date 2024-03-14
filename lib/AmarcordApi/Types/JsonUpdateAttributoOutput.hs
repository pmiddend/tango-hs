-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the types generated from the schema JsonUpdateAttributoOutput
module AmarcordApi.Types.JsonUpdateAttributoOutput where

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

-- | Defines the object schema located at @components.schemas.JsonUpdateAttributoOutput@ in the specification.
-- 
-- 
data JsonUpdateAttributoOutput = JsonUpdateAttributoOutput {
  -- | id
  jsonUpdateAttributoOutputId :: GHC.Types.Int
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON JsonUpdateAttributoOutput
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (["id" Data.Aeson.Types.ToJSON..= jsonUpdateAttributoOutputId obj] : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (["id" Data.Aeson.Types.ToJSON..= jsonUpdateAttributoOutputId obj] : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON JsonUpdateAttributoOutput
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "JsonUpdateAttributoOutput" (\obj -> GHC.Base.pure JsonUpdateAttributoOutput GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "id"))}
-- | Create a new 'JsonUpdateAttributoOutput' with all required fields.
mkJsonUpdateAttributoOutput :: GHC.Types.Int -- ^ 'jsonUpdateAttributoOutputId'
  -> JsonUpdateAttributoOutput
mkJsonUpdateAttributoOutput jsonUpdateAttributoOutputId = JsonUpdateAttributoOutput{jsonUpdateAttributoOutputId = jsonUpdateAttributoOutputId}