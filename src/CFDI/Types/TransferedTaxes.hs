module CFDI.Types.TransferedTaxes where

import CFDI.Types.TransferedTax
import CFDI.XmlNode

data TransferedTaxes = TransferedTaxes [TransferedTax] deriving (Eq, Show)

instance XmlNode TransferedTaxes where
  parseNode n = do
    traTaxes <- parseChildren "Traslado" n
    case traTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Traslado"
      _  -> Right $ TransferedTaxes traTaxes