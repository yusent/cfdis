module CFDI.Types.YesNo where

import CFDI.Chainable
import CFDI.Types.Type

data YesNo = Yes | No deriving (Eq, Show)

instance Chainable YesNo where
  chain Yes = "Sí"
  chain No = "No"

instance Type YesNo where
  parseExpr "Sí" = Right Yes
  parseExpr "No" = Right No
  parseExpr e    = Left  $ InvalidValue e

  render Yes = "Sí"
  render No = "No"
