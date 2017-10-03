module CFDI.Types.RetainedTaxes where

import CFDI.Types.RetainedTax
import CFDI.XmlNode

data RetainedTaxes = RetainedTaxes [RetainedTax] deriving (Eq, Show)

instance XmlNode RetainedTaxes where
  parseNode n = do
    retTaxes <- parseChildren "Retencion" n
    case retTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Retencion"
      _  -> Right $ RetainedTaxes retTaxes
