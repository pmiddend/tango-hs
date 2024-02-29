module AmarcordApi.Types.JsonUserConfig where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified AmarcordApi.Common
data JsonUserConfig
instance Show JsonUserConfig
instance Eq JsonUserConfig
instance FromJSON JsonUserConfig
instance ToJSON JsonUserConfig
