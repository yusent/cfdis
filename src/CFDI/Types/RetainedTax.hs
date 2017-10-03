module CFDI.Types.RetainedTax where

import CFDI.Types.Amount
import CFDI.Types.FactorType
import CFDI.Types.Tax
import CFDI.Types.TaxBase
import CFDI.Types.TaxRate
import CFDI.XmlNode

data RetainedTax = RetainedTax
  { retAmount     :: Amount
  , retBase       :: TaxBase
  , retFactorType :: FactorType
  , retRate       :: TaxRate
  , retTax        :: Tax
  } deriving (Eq, Show)

instance XmlNode RetainedTax where
  parseNode n = RetainedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "Base" n
    <*> requireAttribute "TipoFactor" n
    <*> requireAttribute "TasaOCuota" n
    <*> requireAttribute "Impuesto" n
