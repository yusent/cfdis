module CFDI.Types.WaybillComplementGoods where

import CFDI.Chainable
import CFDI.Types.WaybillComplementGood
import CFDI.XmlNode
import Data.Text (intercalate)

data WaybillComplementGoods = WaybillComplementGoods
  { wcGoods :: [WaybillComplementGood]
  } deriving (Eq, Show)

instance Chainable WaybillComplementGoods where
  chain = intercalate "|" . map chain . wcGoods

instance XmlNode WaybillComplementGoods where
  children = map renderNode . wcGoods

  nodeName = const "Mercancias"

  parseNode n = do
    locs <- parseChildren "Mercancia" n
    case locs of
      [] -> Left  $ ExpectedAtLeastOne "Mercancia"
      _  -> Right $ WaybillComplementGoods locs
