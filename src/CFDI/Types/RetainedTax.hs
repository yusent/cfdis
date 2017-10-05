module CFDI.Types.RetainedTax where

import CFDI.Chainable
import CFDI.Types.Amount
import CFDI.Types.Tax
import CFDI.XmlNode

data RetainedTax = RetainedTax
  { retAmount :: Amount
  , retTax    :: Tax
  } deriving (Eq, Show)

instance Chainable RetainedTax where
  chain c = retTax
        <@> retAmount
        <~> (c, "")

instance XmlNode RetainedTax where
  attributes n =
    [ attr "Importe"  $ retAmount n
    , attr "Impuesto" $ retTax n
    ]

  nodeName = const "Retencion"

  parseNode n = RetainedTax
    <$> requireAttribute "Importe" n
    <*> requireAttribute "Impuesto" n
