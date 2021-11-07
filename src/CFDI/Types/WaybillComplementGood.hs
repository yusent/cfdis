module CFDI.Types.WaybillComplementGood where

import CFDI.Chainable
import CFDI.XmlNode
import CFDI.Types.ProductOrService
import Data.Maybe (catMaybes)

data WaybillComplementGood = WaybillComplementGood
  { wcGoodCode :: Maybe ProductOrService
  } deriving (Eq, Show)

instance Chainable WaybillComplementGood where
  chain _ = ""

instance XmlNode WaybillComplementGood where
  attributes good = catMaybes
    [ attr "BienesTransp" <$> wcGoodCode good
    ]

  children n = catMaybes
    [
    ]

  nodeName = const "Mercancia"

  parseNode n = WaybillComplementGood
    <$> parseAttribute "BienesTransp" n
