module CFDI.Types.PaymentMethod where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text       (pack)

data PaymentMethod
  = OneTimePayment
  | PartialPayment
  deriving (Eq, Show)

instance Chainable PaymentMethod where
  chain = pack . render

instance Type PaymentMethod where
  parseExpr "PUE" = Right OneTimePayment
  parseExpr "PPD" = Right PartialPayment
  parseExpr _     = Left NotInCatalog

  render OneTimePayment = "PUE"
  render PartialPayment = "PPD"
