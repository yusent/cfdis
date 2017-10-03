module CFDI.Types.ConceptTransferedTax where

import CFDI.Types.Amount
import CFDI.Types.FactorType
import CFDI.Types.Tax
import CFDI.Types.TaxBase
import CFDI.Types.TaxRate
import CFDI.XmlNode

data ConceptTransferedTax = ConceptTransferedTax
  { conTransAmount     :: Amount
  , conTransBase       :: TaxBase
  , conTransFactorType :: FactorType
  , conTransRate       :: TaxRate
  , conTransTax        :: Tax
  } deriving (Eq, Show)

instance XmlNode ConceptTransferedTax where
  parseNode n = ConceptTransferedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "Base" n
    <*> requireAttribute "TipoFactor" n
    <*> requireAttribute "TasaOCuota" n
    <*> requireAttribute "Impuesto" n
