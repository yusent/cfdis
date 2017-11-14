module CFDI.Types.PaymentRetainedTaxes where

import CFDI.Chainable
import CFDI.Types.PaymentRetainedTax
import CFDI.XmlNode
import Data.Text                     (intercalate)

newtype PaymentRetainedTaxes = PaymentRetainedTaxes [PaymentRetainedTax]
  deriving (Eq, Show)

instance Chainable PaymentRetainedTaxes where
  chain (PaymentRetainedTaxes ts) = intercalate "|" $ chain <$> ts

instance XmlNode PaymentRetainedTaxes where
  children (PaymentRetainedTaxes ts) = renderNode <$> ts

  nodeName = const "Retenciones"

  nodePrefix = const "pago10"

  parseNode n = do
    retTaxes <- parseChildren "Retencion" n
    case retTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Retencion"
      _  -> Right $ PaymentRetainedTaxes retTaxes
