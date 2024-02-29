module AmarcordApi.Types.HTTPValidationError where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified AmarcordApi.Common
data HTTPValidationError
instance Show HTTPValidationError
instance Eq HTTPValidationError
instance FromJSON HTTPValidationError
instance ToJSON HTTPValidationError
