module CFDI.Types.PaymentMethods where

import CFDI.Types.Catalog

data PaymentMethod
  = OneTimePayment
  | PartialPayment

instance Catalog PaymentMethod where
  fromCode "PUE" = Just OneTimePayment
  fromCode "PPD" = Just PartialPayment
  fromCode _     = Nothing

  toCode OneTimePayment = "PUE"
  toCode PartialPayment = "PPD"
