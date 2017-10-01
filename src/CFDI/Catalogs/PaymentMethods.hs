module CFDI.Catalogs.PaymentMethods where

import CFDI.Catalog

data PaymentMethod
  = OneTimePayment
  | PartialPayment

instance Catalog PaymentMethod where
  fromCode "PUE" = Just OneTimePayment
  fromCode "PPD" = Just PartialPayment
  fromCode _     = Nothing

  toCode OneTimePayment = "PUE"
  toCode PartialPayment = "PPD"
