module AmarcordApi.Types.JsonChemical where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified AmarcordApi.Common
data JsonChemical
instance Show JsonChemical
instance Eq JsonChemical
instance FromJSON JsonChemical
instance ToJSON JsonChemical
