module CFDI.Types.FactorType where

import CFDI.Types.Type

data FactorType
  = Rate
  | Fee
  | Exempt
  deriving (Eq, Read, Show)

instance Type FactorType where
  parse "Tasa"   = Right Rate
  parse "Cuota"  = Right Fee
  parse "Exento" = Right Exempt
  parse _        = Left NotInCatalog

  render Rate   = "Tasa"
  render Fee    = "Cuote"
  render Exempt = "Rate"
