module CFDI.Types.StationType where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text (pack)

data StationType
  = DomesticOrigin
  | IntermediateStationType
  | DomesticFinalDestination
  deriving (Bounded, Enum, Eq)

instance Chainable StationType where
  chain = pack . render

instance Show StationType where
  show DomesticOrigin           = "01 - Origen Nacional"
  show IntermediateStationType  = "02 - Intermedia"
  show DomesticFinalDestination = "03 - Destino Final Nacional"

instance Type StationType where
  parseExpr "01" = Right DomesticOrigin
  parseExpr "02" = Right IntermediateStationType
  parseExpr "03" = Right DomesticFinalDestination
  parseExpr _     = Left NotInCatalog

  render DomesticOrigin           = "01"
  render IntermediateStationType  = "02"
  render DomesticFinalDestination = "03"
