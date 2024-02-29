module AmarcordApi.Types.JSONSchemaString where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified AmarcordApi.Common
data JSONSchemaString
instance Show JSONSchemaString
instance Eq JSONSchemaString
instance FromJSON JSONSchemaString
instance ToJSON JSONSchemaString
