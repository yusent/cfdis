module CFDI.Types.InOutWay where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text (pack)

data InOutWay
  = FederalTrucking
  | SeaTransport
  | AirTransport
  | RailTransport
  | Duct
  deriving (Bounded, Enum, Eq)

instance Chainable InOutWay where
  chain = pack . render

instance Show InOutWay where
  show FederalTrucking = "01 - Autotransporte Federal"
  show SeaTransport    = "02 - Transporte Marítimo"
  show AirTransport    = "03 - Transporte Aéreo"
  show RailTransport   = "04 - Transporte Ferroviario"
  show Duct            = "05 - Ducto"

instance Type InOutWay where
  parseExpr "01" = Right FederalTrucking
  parseExpr "02" = Right SeaTransport
  parseExpr "03" = Right AirTransport
  parseExpr "04" = Right RailTransport
  parseExpr "05" = Right Duct
  parseExpr _     = Left NotInCatalog

  render FederalTrucking = "01"
  render SeaTransport    = "02"
  render AirTransport    = "03"
  render RailTransport   = "04"
  render Duct            = "05"
