module CFDI.Types.PaymentRelatedDocument where

import CFDI.Chainable
import CFDI.XmlNode

data PaymentRelatedDocument = PaymentRelatedDocument deriving (Eq, Show)

instance Chainable PaymentRelatedDocument where
  chain PaymentRelatedDocument = ""

instance XmlNode PaymentRelatedDocument where
  nodeName = const "DoctoRelacionado"

  nodePrefix = const "pago10"

  parseNode _ = Right PaymentRelatedDocument
