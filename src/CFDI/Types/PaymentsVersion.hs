module CFDI.Types.PaymentsVersion where

import CFDI.Chainable
import CFDI.Types.Type

newtype PaymentsVersion = PaymentsVersion Float deriving (Eq, Show)

instance Chainable PaymentsVersion where
  chain (PaymentsVersion v) = chain v

instance Type PaymentsVersion where
  parseExpr "2.0" = Right $ PaymentsVersion 2.0
  parseExpr "1.0" = Right $ PaymentsVersion 1.0
  parseExpr e     = Left  $ InvalidValue e

  render (PaymentsVersion 2.0) = "2.0"
  render _ = "1.0"
