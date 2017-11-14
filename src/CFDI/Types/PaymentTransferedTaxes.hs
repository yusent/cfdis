module CFDI.Types.PaymentTransferedTaxes where

import CFDI.Chainable
import CFDI.Types.PaymentTransferedTax
import CFDI.XmlNode
import Data.Text                       (intercalate)

newtype PaymentTransferedTaxes = PaymentTransferedTaxes [PaymentTransferedTax]
  deriving (Eq, Show)

instance Chainable PaymentTransferedTaxes where
  chain (PaymentTransferedTaxes ts) = intercalate "|" $ chain <$> ts

instance XmlNode PaymentTransferedTaxes where
  children (PaymentTransferedTaxes ts) = renderNode <$> ts

  nodeName = const "Traslados"

  nodePrefix = const "pago10"

  parseNode n = do
    traTaxes <- parseChildren "Traslado" n
    case traTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Traslado"
      _  -> Right $ PaymentTransferedTaxes traTaxes
