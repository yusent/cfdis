module CFDI.Types.PaymentMethod where

import CFDI.Types.Type

data PaymentMethod
  = OneTimePayment
  | PartialPayment
  deriving (Eq, Show)

instance Type PaymentMethod where
  parse "PUE" = Right OneTimePayment
  parse "PPD" = Right PartialPayment
  parse _     = Left NotInCatalog

  render OneTimePayment = "PUE"
  render PartialPayment = "PPD"
