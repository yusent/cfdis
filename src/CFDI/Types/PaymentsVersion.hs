module CFDI.Types.PaymentsVersion where

import CFDI.Chainable
import CFDI.Types.Type

newtype PaymentsVersion = PaymentsVersion Float deriving (Eq, Show)

instance Chainable PaymentsVersion where
  chain (PaymentsVersion v) = chain v

instance Type PaymentsVersion where
  parseExpr "1.0" = Right $ PaymentsVersion 1.0
  parseExpr e     = Left  $ InvalidValue e

  render _ = "1.0"
