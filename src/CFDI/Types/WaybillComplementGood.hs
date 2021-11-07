module CFDI.Types.WaybillComplementGood where

import CFDI.Chainable
import CFDI.XmlNode
import CFDI.Types.ProductOrService
import CFDI.Types.STCCID
import Data.Maybe (catMaybes)

data WaybillComplementGood = WaybillComplementGood
  { wcGoodCode :: Maybe ProductOrService
  , wcGoodSTCCCode :: Maybe STCCID
  } deriving (Eq, Show)

instance Chainable WaybillComplementGood where
  chain _ = ""

instance XmlNode WaybillComplementGood where
  attributes n = catMaybes
    [ attr "BienesTransp" <$> wcGoodCode n
    , attr "ClaveSTCC" <$> wcGoodSTCCCode n
    ]

  children n = catMaybes
    [
    ]

  nodeName = const "Mercancia"

  parseNode n = WaybillComplementGood
    <$> parseAttribute "BienesTransp" n
    <*> parseAttribute "ClaveSTCC" n
