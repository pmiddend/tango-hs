-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the different functions to run the operation readRunsApiRuns_BeamtimeId_Get
module AmarcordApi.Operations.ReadRunsApiRunsBeamtimeIdGet where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Control.Monad.Fail
import qualified Control.Monad.Trans.Reader
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Encoding.Internal
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.ByteString
import qualified Data.ByteString as Data.ByteString.Internal
import qualified Data.ByteString as Data.ByteString.Internal.Type
import qualified Data.Either
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text as Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified Data.Vector
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client as Network.HTTP.Client.Request
import qualified Network.HTTP.Client as Network.HTTP.Client.Types
import qualified Network.HTTP.Simple
import qualified Network.HTTP.Types
import qualified Network.HTTP.Types as Network.HTTP.Types.Status
import qualified Network.HTTP.Types as Network.HTTP.Types.URI
import qualified AmarcordApi.Common
import AmarcordApi.Types

-- | > GET /api/runs/{beamtimeId}
-- 
-- Read Runs
readRunsApiRuns_BeamtimeId_Get :: forall m . AmarcordApi.Common.MonadHTTP m => ReadRunsApiRunsBeamtimeIdGetParameters -- ^ Contains all available parameters of this operation (query and path parameters)
  -> AmarcordApi.Common.ClientT m (Network.HTTP.Client.Types.Response ReadRunsApiRunsBeamtimeIdGetResponse) -- ^ Monadic computation which returns the result of the operation
readRunsApiRuns_BeamtimeId_Get parameters = GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either ReadRunsApiRunsBeamtimeIdGetResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> ReadRunsApiRunsBeamtimeIdGetResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                                                               JsonReadRuns)
                                                                                                                                                                                                   | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 422) (Network.HTTP.Client.Types.responseStatus response) -> ReadRunsApiRunsBeamtimeIdGetResponse422 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                                                               HTTPValidationError)
                                                                                                                                                                                                   | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0) (AmarcordApi.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/api/runs/" GHC.Base.<> (AmarcordApi.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (AmarcordApi.Common.textToByte GHC.Base.$ AmarcordApi.Common.stringifyModel (readRunsApiRunsBeamtimeIdGetParametersPathBeamtimeId parameters))) GHC.Base.<> "")) [AmarcordApi.Common.QueryParameter (Data.Text.Internal.pack "date") (Data.Aeson.Types.ToJSON.toJSON Data.Functor.<$> readRunsApiRunsBeamtimeIdGetParametersQueryDate parameters) (Data.Text.Internal.pack "form") GHC.Types.False,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      AmarcordApi.Common.QueryParameter (Data.Text.Internal.pack "filter") (Data.Aeson.Types.ToJSON.toJSON Data.Functor.<$> readRunsApiRunsBeamtimeIdGetParametersQueryFilter parameters) (Data.Text.Internal.pack "form") GHC.Types.False])
-- | Defines the object schema located at @paths.\/api\/runs\/{beamtimeId}.GET.parameters@ in the specification.
-- 
-- 
data ReadRunsApiRunsBeamtimeIdGetParameters = ReadRunsApiRunsBeamtimeIdGetParameters {
  -- | pathBeamtimeId: Represents the parameter named \'beamtimeId\'
  readRunsApiRunsBeamtimeIdGetParametersPathBeamtimeId :: GHC.Types.Int
  -- | queryDate: Represents the parameter named \'date\'
  , readRunsApiRunsBeamtimeIdGetParametersQueryDate :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  -- | queryFilter: Represents the parameter named \'filter\'
  , readRunsApiRunsBeamtimeIdGetParametersQueryFilter :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON ReadRunsApiRunsBeamtimeIdGetParameters
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (["pathBeamtimeId" Data.Aeson.Types.ToJSON..= readRunsApiRunsBeamtimeIdGetParametersPathBeamtimeId obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("queryDate" Data.Aeson.Types.ToJSON..=)) (readRunsApiRunsBeamtimeIdGetParametersQueryDate obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("queryFilter" Data.Aeson.Types.ToJSON..=)) (readRunsApiRunsBeamtimeIdGetParametersQueryFilter obj) : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (["pathBeamtimeId" Data.Aeson.Types.ToJSON..= readRunsApiRunsBeamtimeIdGetParametersPathBeamtimeId obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("queryDate" Data.Aeson.Types.ToJSON..=)) (readRunsApiRunsBeamtimeIdGetParametersQueryDate obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("queryFilter" Data.Aeson.Types.ToJSON..=)) (readRunsApiRunsBeamtimeIdGetParametersQueryFilter obj) : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON ReadRunsApiRunsBeamtimeIdGetParameters
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "ReadRunsApiRunsBeamtimeIdGetParameters" (\obj -> ((GHC.Base.pure ReadRunsApiRunsBeamtimeIdGetParameters GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "pathBeamtimeId")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "queryDate")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "queryFilter"))}
-- | Create a new 'ReadRunsApiRunsBeamtimeIdGetParameters' with all required fields.
mkReadRunsApiRunsBeamtimeIdGetParameters :: GHC.Types.Int -- ^ 'readRunsApiRunsBeamtimeIdGetParametersPathBeamtimeId'
  -> ReadRunsApiRunsBeamtimeIdGetParameters
mkReadRunsApiRunsBeamtimeIdGetParameters readRunsApiRunsBeamtimeIdGetParametersPathBeamtimeId = ReadRunsApiRunsBeamtimeIdGetParameters{readRunsApiRunsBeamtimeIdGetParametersPathBeamtimeId = readRunsApiRunsBeamtimeIdGetParametersPathBeamtimeId,
                                                                                                                                       readRunsApiRunsBeamtimeIdGetParametersQueryDate = GHC.Maybe.Nothing,
                                                                                                                                       readRunsApiRunsBeamtimeIdGetParametersQueryFilter = GHC.Maybe.Nothing}
-- | Represents a response of the operation 'readRunsApiRuns_BeamtimeId_Get'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'ReadRunsApiRunsBeamtimeIdGetResponseError' is used.
data ReadRunsApiRunsBeamtimeIdGetResponse =
   ReadRunsApiRunsBeamtimeIdGetResponseError GHC.Base.String -- ^ Means either no matching case available or a parse error
  | ReadRunsApiRunsBeamtimeIdGetResponse200 JsonReadRuns -- ^ Successful Response
  | ReadRunsApiRunsBeamtimeIdGetResponse422 HTTPValidationError -- ^ Validation Error
  deriving (GHC.Show.Show, GHC.Classes.Eq)