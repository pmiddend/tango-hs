module AmarcordApi.Types.JsonEventInput where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified AmarcordApi.Common
data JsonEventInput
instance Show JsonEventInput
instance Eq JsonEventInput
instance FromJSON JsonEventInput
instance ToJSON JsonEventInput
