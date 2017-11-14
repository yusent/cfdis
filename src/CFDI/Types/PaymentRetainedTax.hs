module CFDI.Types.PaymentRetainedTax where

import CFDI.Chainable
import CFDI.Types.Amount
import CFDI.Types.Tax
import CFDI.XmlNode

data PaymentRetainedTax = PaymentRetainedTax
  { pretAmount :: Amount
  , pretTax    :: Tax
  } deriving (Eq, Show)

instance Chainable PaymentRetainedTax where
  chain c = pretTax
        <@> pretAmount
        <~> (c, "")

instance XmlNode PaymentRetainedTax where
  attributes n =
    [ attr "Importe"  $ pretAmount n
    , attr "Impuesto" $ pretTax n
    ]

  nodeName = const "Retencion"

  nodePrefix = const "pago10"

  parseNode n = PaymentRetainedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "Impuesto" n
