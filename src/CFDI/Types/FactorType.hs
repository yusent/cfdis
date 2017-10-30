module CFDI.Types.FactorType where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text       (pack)

data FactorType
  = Rate
  | Fee
  | Exempt
  deriving (Eq, Read, Show)

instance Chainable FactorType where
  chain = pack . render

instance Type FactorType where
  parseExpr "Tasa"   = Right Rate
  parseExpr "Cuota"  = Right Fee
  parseExpr "Exento" = Right Exempt
  parseExpr _        = Left NotInCatalog

  render Rate   = "Tasa"
  render Fee    = "Cuota"
  render Exempt = "Exento"
