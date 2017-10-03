module CFDI.Types.RetainedTax where

import CFDI.Types.Amount
import CFDI.Types.Tax
import CFDI.XmlNode

data RetainedTax = RetainedTax
  { retAmount :: Amount
  , retTax    :: Tax
  } deriving (Eq, Show)

instance XmlNode RetainedTax where
  parseNode n = RetainedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "Impuesto" n
