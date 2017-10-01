module CFDI.Types.Tax where

import CFDI.Types.Type

data Tax
  = ISR
  | IVA
  | IEPS
  deriving (Eq, Read, Show)

instance Type Tax where
  parse "001" = Right ISR
  parse "002" = Right IVA
  parse "003" = Right IEPS
  parse _     = Left NotInCatalog

  render ISR  = "001"
  render IVA  = "002"
  render IEPS = "003"
