module CFDI.Types.TransferedTax where

import CFDI.Types.Amount
import CFDI.Types.FactorType
import CFDI.Types.Tax
import CFDI.Types.TaxRate
import CFDI.XmlNode

data TransferedTax = TransferedTax
  { transAmount     :: Amount
  , transFactorType :: FactorType
  , transRate       :: TaxRate
  , transTax        :: Tax
  } deriving (Eq, Show)

instance XmlNode TransferedTax where
  parseNode n = TransferedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "TipoFactor" n
    <*> requireAttribute "TasaOCuota" n
    <*> requireAttribute "Impuesto" n
