module CFDI.Types.PaymentTaxes where

import CFDI.Chainable
import CFDI.Types.Amount
import CFDI.Types.PaymentRetainedTaxes
import CFDI.Types.PaymentTransferedTaxes
import CFDI.XmlNode
import Data.Maybe                        (catMaybes)

data PaymentTaxes = PaymentTaxes
  { ptRetainedTaxes   :: Maybe PaymentRetainedTaxes
  , ptTotalTransfered :: Maybe Amount
  , ptTotalRetained   :: Maybe Amount
  , ptTransferedTaxes :: Maybe PaymentTransferedTaxes
  } deriving (Eq, Show)

instance Chainable PaymentTaxes where
  chain _ = ""

instance XmlNode PaymentTaxes where
  attributes n = catMaybes
    [ attr "TotalImpuestosRetenidos"   <$> ptTotalRetained n
    , attr "TotalImpuestosTrasladados" <$> ptTotalTransfered n
    ]

  children n = catMaybes
    [ renderNode <$> ptRetainedTaxes n
    , renderNode <$> ptTransferedTaxes n
    ]

  nodeName = const "Impuestos"

  nodePrefix = const "pago10"

  parseNode n = PaymentTaxes
    <$> parseChild "Retenciones" n
    <*> parseAttribute "TotalImpuestosTrasladados" n
    <*> parseAttribute "TotalImpuestosRetenidos" n
    <*> parseChild "Traslados" n
