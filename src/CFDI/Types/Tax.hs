module CFDI.Types.Tax where

import CFDI.Types.Type

data Tax
  = ISR
  | IVA
  | IEPS
  deriving (Eq, Read, Show)

instance Type Tax where
  parseExpr "001" = Right ISR
  parseExpr "002" = Right IVA
  parseExpr "003" = Right IEPS
  parseExpr _     = Left NotInCatalog

  render ISR  = "001"
  render IVA  = "002"
  render IEPS = "003"
