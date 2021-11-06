module CFDI.Types.HarborType where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text (pack)

data HarborType
  = DeepSea
  | Coastal
  deriving (Bounded, Enum, Eq)

instance Chainable HarborType where
  chain = pack . render

instance Show HarborType where
  show DeepSea = "Altura"
  show Coastal = "Cabotaje"

instance Type HarborType where
  parseExpr "Altura" = Right DeepSea
  parseExpr "Cabotaje" = Right Coastal
  parseExpr _     = Left NotInCatalog

  render = show
