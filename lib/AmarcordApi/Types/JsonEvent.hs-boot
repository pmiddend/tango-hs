module AmarcordApi.Types.JsonEvent where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified AmarcordApi.Common
data JsonEvent
instance Show JsonEvent
instance Eq JsonEvent
instance FromJSON JsonEvent
instance ToJSON JsonEvent
