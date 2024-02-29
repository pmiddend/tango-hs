module AmarcordApi.Types.JsonDataSet where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified AmarcordApi.Common
data JsonDataSet
instance Show JsonDataSet
instance Eq JsonDataSet
instance FromJSON JsonDataSet
instance ToJSON JsonDataSet
