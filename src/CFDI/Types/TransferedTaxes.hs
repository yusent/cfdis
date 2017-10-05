module CFDI.Types.TransferedTaxes where

import CFDI.Chainable
import CFDI.Types.TransferedTax
import CFDI.XmlNode
import Data.Text                (intercalate)

newtype TransferedTaxes = TransferedTaxes [TransferedTax] deriving (Eq, Show)

instance Chainable TransferedTaxes where
  chain (TransferedTaxes ts) = intercalate "|" $ chain <$> ts

instance XmlNode TransferedTaxes where
  children (TransferedTaxes ts) = renderNode <$> ts

  nodeName = const "Traslados"

  parseNode n = do
    traTaxes <- parseChildren "Traslado" n
    case traTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Traslado"
      _  -> Right $ TransferedTaxes traTaxes
