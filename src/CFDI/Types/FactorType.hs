module CFDI.Types.FactorType where

import CFDI.Types.Type

data FactorType
  = Rate
  | Fee
  | Exempt
  deriving (Eq, Read, Show)

instance Type FactorType where
  parseExpr "Tasa"   = Right Rate
  parseExpr "Cuota"  = Right Fee
  parseExpr "Exento" = Right Exempt
  parseExpr _        = Left NotInCatalog

  render Rate   = "Tasa"
  render Fee    = "Cuote"
  render Exempt = "Rate"
