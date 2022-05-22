module CFDI.Types.Periodicity where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text (pack)

data Periodicity
  = Daily
  | Weekly
  | Biweekly
  | Monthly
  | Bimonthly
  deriving (Bounded, Enum, Eq)

instance Chainable Periodicity where
  chain = pack . render

instance Show Periodicity where
  show Daily     = "01 - Diario"
  show Weekly    = "02 - Semanal"
  show Biweekly  = "03 - Quincenal"
  show Monthly   = "04 - Mensual"
  show Bimonthly = "05 - Bimestral"

instance Type Periodicity where
  parseExpr "01" = Right Daily
  parseExpr "02" = Right Weekly
  parseExpr "03" = Right Biweekly
  parseExpr "04" = Right Monthly
  parseExpr "05" = Right Bimonthly
  parseExpr _     = Left NotInCatalog

  render Daily     = "01"
  render Weekly    = "02"
  render Biweekly  = "03"
  render Monthly   = "04"
  render Bimonthly = "05"
