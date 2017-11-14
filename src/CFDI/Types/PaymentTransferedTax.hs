module CFDI.Types.PaymentTransferedTax where

import CFDI.Chainable
import CFDI.Types.Amount
import CFDI.Types.FactorType
import CFDI.Types.Tax
import CFDI.Types.TaxRate
import CFDI.XmlNode

data PaymentTransferedTax = PaymentTransferedTax
  { ptransAmount     :: Amount
  , ptransFactorType :: FactorType
  , ptransRate       :: TaxRate
  , ptransTax        :: Tax
  } deriving (Eq, Show)

instance Chainable PaymentTransferedTax where
  chain c = ptransTax
        <@> ptransFactorType
        <~> ptransRate
        <~> ptransAmount
        <~> (c, "")

instance XmlNode PaymentTransferedTax where
  attributes n =
    [ attr "Importe"    $ ptransAmount n
    , attr "TipoFactor" $ ptransFactorType n
    , attr "TasaOCuota" $ ptransRate n
    , attr "Impuesto"   $ ptransTax n
    ]

  nodeName = const "Traslado"

  nodePrefix = const "pago10"

  parseNode n = PaymentTransferedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "TipoFactor" n
    <*> requireAttribute "TasaOCuota" n
    <*> requireAttribute "Impuesto" n
