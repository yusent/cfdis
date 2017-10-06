module CFDI.Types.ConceptTransferedTax where

import CFDI.Chainable
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

instance Chainable ConceptTransferedTax where
  chain c = conTransBase
        <@> conTransTax
        <~> conTransFactorType
        <~> conTransRate
        <~> conTransAmount
        <~> (c, "")

instance XmlNode ConceptTransferedTax where
  attributes n =
    [ attr "Importe"    $ conTransAmount n
    , attr "Base"       $ conTransBase n
    , attr "TipoFactor" $ conTransFactorType n
    , attr "TasaOCuota" $ conTransRate n
    , attr "Impuesto"   $ conTransTax n
    ]

  nodeName = const "Traslado"

  parseNode n = ConceptTransferedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "Base" n
    <*> requireAttribute "TipoFactor" n
    <*> requireAttribute "TasaOCuota" n
    <*> requireAttribute "Impuesto" n
