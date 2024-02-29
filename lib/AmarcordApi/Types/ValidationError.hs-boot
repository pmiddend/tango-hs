module AmarcordApi.Types.ValidationError where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified AmarcordApi.Common
data ValidationError
instance Show ValidationError
instance Eq ValidationError
instance FromJSON ValidationError
instance ToJSON ValidationError
