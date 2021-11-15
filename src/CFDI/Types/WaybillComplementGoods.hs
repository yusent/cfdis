module CFDI.Types.WaybillComplementGoods where

import CFDI.Chainable
import CFDI.Types.WaybillComplementFederalTrucking
import CFDI.Types.WaybillComplementGood
import CFDI.XmlNode
import Data.Maybe (catMaybes)
import Data.Text (intercalate)

data WaybillComplementGoods = WaybillComplementGoods
  { wcGoods :: [WaybillComplementGood]
  , wcGoodsFederalTrucking :: Maybe WaybillComplementFederalTrucking
  } deriving (Eq, Show)

instance Chainable WaybillComplementGoods where
  chain = intercalate "|" . map chain . wcGoods

instance XmlNode WaybillComplementGoods where
  children n = catMaybes
    [ renderNode <$> wcGoodsFederalTrucking n
    ] ++ map renderNode (wcGoods n)

  nodeName = const "Mercancias"

  parseNode n = do
    locs <- parseChildren "Mercancia" n
    case locs of
      [] -> Left  $ ExpectedAtLeastOne "Mercancia"
      _  -> WaybillComplementGoods locs
        <$> parseChild "AutotransporteFederal" n
