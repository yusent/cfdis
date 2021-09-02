module CFDI.Types.WaybillComplementLocations where

import CFDI.Chainable
import CFDI.Types.WaybillComplementLocation
import CFDI.XmlNode
import Data.Text (intercalate)

data WaybillComplementLocations = WaybillComplementLocations
  { wcLocations :: [WaybillComplementLocation]
  } deriving (Eq, Show)

instance Chainable WaybillComplementLocations where
  chain = intercalate "|" . map chain . wcLocations

instance XmlNode WaybillComplementLocations where
  children = map renderNode . wcLocations

  nodeName = const "Ubicaciones"

  parseNode n = do
    locs <- parseChildren "Ubicacion" n
    case locs of
      [] -> Left  $ ExpectedAtLeastOne "Ubicacion"
      _  -> Right $ WaybillComplementLocations locs
