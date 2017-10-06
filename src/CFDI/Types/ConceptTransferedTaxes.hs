module CFDI.Types.ConceptTransferedTaxes where

import CFDI.Chainable
import CFDI.Types.ConceptTransferedTax
import CFDI.XmlNode
import Data.Text                       (intercalate)

newtype ConceptTransferedTaxes =
  ConceptTransferedTaxes [ConceptTransferedTax] deriving (Eq, Show)

instance Chainable ConceptTransferedTaxes where
  chain (ConceptTransferedTaxes ts) = intercalate "|" $ chain <$> ts

instance XmlNode ConceptTransferedTaxes where
  children (ConceptTransferedTaxes ts) = renderNode <$> ts

  nodeName = const "Traslados"

  parseNode n = do
    traTaxes <- parseChildren "Traslado" n
    case traTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Traslado"
      _  -> Right $ ConceptTransferedTaxes traTaxes
