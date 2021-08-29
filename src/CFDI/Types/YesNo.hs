module CFDI.Types.YesNo where

import CFDI.Chainable
import CFDI.Types.Type

newtype YesNo = YesNo Bool deriving (Eq, Show)

instance Chainable YesNo where
  chain (YesNo v) = if v then "Sí" else "No"

instance Type YesNo where
  parseExpr "Sí" = Right $ YesNo True
  parseExpr "No" = Right $ YesNo False
  parseExpr e    = Left  $ InvalidValue e

  render (YesNo v) = if v then "Sí" else "No"
