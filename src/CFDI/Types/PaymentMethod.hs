module CFDI.Types.PaymentMethod where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text       (pack)

data PaymentMethod
  = OneTimePayment
  | PartialPayment
  deriving (Bounded, Enum, Eq)

instance Chainable PaymentMethod where
  chain = pack . render

instance Show PaymentMethod where
  show OneTimePayment = "PUE - Pago en una sola exhibición"
  show PartialPayment = "PPD - Pago en parcialidades o diferido"

instance Type PaymentMethod where
  parseExpr "PUE" = Right OneTimePayment
  parseExpr "PPD" = Right PartialPayment
  parseExpr _     = Left NotInCatalog

  render OneTimePayment = "PUE"
  render PartialPayment = "PPD"
