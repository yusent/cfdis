module CFDI.Types.ConceptTransferedTaxes where

import CFDI.Types.ConceptTransferedTax
import CFDI.XmlNode

data ConceptTransferedTaxes =
  ConceptTransferedTaxes [ConceptTransferedTax] deriving (Eq, Show)

instance XmlNode ConceptTransferedTaxes where
  parseNode n = do
    traTaxes <- parseChildren "Traslado" n
    case traTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Traslado"
      _  -> Right $ ConceptTransferedTaxes traTaxes