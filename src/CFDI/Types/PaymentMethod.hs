module CFDI.Types.PaymentMethod where

import CFDI.Types.Type

data PaymentMethod
  = OneTimePayment
  | PartialPayment
  deriving (Eq, Show)

instance Type PaymentMethod where
  parseExpr "PUE" = Right OneTimePayment
  parseExpr "PPD" = Right PartialPayment
  parseExpr _     = Left NotInCatalog

  render OneTimePayment = "PUE"
  render PartialPayment = "PPD"
