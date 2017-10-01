module CFDI.Catalogs.Taxes where

import CFDI.Catalog

data Tax
  = ISR
  | IVA
  | IEPS
  deriving (Eq, Read, Show)

instance Catalog Tax where
  fromCode "001" = Just ISR
  fromCode "002" = Just IVA
  fromCode "003" = Just IEPS
  fromCode _     = Nothing

  toCode ISR  = "001"
  toCode IVA  = "002"
  toCode IEPS = "003"
