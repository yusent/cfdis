module CFDI.Types.FactorTypes where

import CFDI.Types.Catalog

data FactorType
  = Rate
  | Fee
  | Exempt
  deriving (Eq, Read, Show)

instance Catalog FactorType where
  fromCode "Tasa"   = Just Rate
  fromCode "Cuota"  = Just Fee
  fromCode "Exento" = Just Exempt
  fromCode _        = Nothing

  toCode Rate   = "Tasa"
  toCode Fee    = "Cuote"
  toCode Exempt = "Rate"
