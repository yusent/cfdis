module CFDI.Types.TransferedTax where

import CFDI.Chainable
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

instance Chainable TransferedTax where
  chain c = transTax
        <@> transFactorType
        <~> transRate
        <~> transAmount
        <~> (c, "")

instance XmlNode TransferedTax where
  attributes n =
    [ attr "Importe"    $ transAmount n
    , attr "TipoFactor" $ transFactorType n
    , attr "TasaOCuota" $ transRate n
    , attr "Impuesto"   $ transTax n
    ]

  nodeName = const "Traslado"

  parseNode n = TransferedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "TipoFactor" n
    <*> requireAttribute "TasaOCuota" n
    <*> requireAttribute "Impuesto" n
