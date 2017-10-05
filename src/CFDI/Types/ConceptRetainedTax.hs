module CFDI.Types.ConceptRetainedTax where

import CFDI.Types.Amount
import CFDI.Types.FactorType
import CFDI.Types.Tax
import CFDI.Types.TaxBase
import CFDI.Types.TaxRate
import CFDI.XmlNode

data ConceptRetainedTax = ConceptRetainedTax
  { conRetAmount     :: Amount
  , conRetBase       :: TaxBase
  , conRetFactorType :: FactorType
  , conRetRate       :: TaxRate
  , conRetTax        :: Tax
  } deriving (Eq, Show)

instance XmlNode ConceptRetainedTax where
  attributes n =
    [ attr "Importe"    $ conRetAmount n
    , attr "Base"       $ conRetBase n
    , attr "TipoFactor" $ conRetFactorType n
    , attr "TasaOCuota" $ conRetRate n
    , attr "Impuesto"   $ conRetTax n
    ]

  nodeName = const "Retencion"

  parseNode n = ConceptRetainedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "Base" n
    <*> requireAttribute "TipoFactor" n
    <*> requireAttribute "TasaOCuota" n
    <*> requireAttribute "Impuesto" n
