module CFDI.Types.ConceptRetainedTaxes where

import CFDI.Types.ConceptRetainedTax
import CFDI.XmlNode

data ConceptRetainedTaxes =
  ConceptRetainedTaxes [ConceptRetainedTax] deriving (Eq, Show)

instance XmlNode ConceptRetainedTaxes where
  children (ConceptRetainedTaxes ts) = renderNode <$> ts

  nodeName = const "Retenciones"

  parseNode n = do
    retTaxes <- parseChildren "Retencion" n
    case retTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Retencion"
      _  -> Right $ ConceptRetainedTaxes retTaxes
