module AmarcordApi.Types.JsonFileOutput where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified AmarcordApi.Common
data JsonFileOutput
instance Show JsonFileOutput
instance Eq JsonFileOutput
instance FromJSON JsonFileOutput
instance ToJSON JsonFileOutput
