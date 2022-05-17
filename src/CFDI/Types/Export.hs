module CFDI.Types.Export where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text (pack)

data Export
  = ExportNotApplicable
  | ExportDefinitive
  | ExportTemporary
  | ExportDefinitiveWithoutDisposal
  deriving (Bounded, Enum, Eq)

instance Chainable Export where
  chain = pack . render

instance Show Export where
  show ExportNotApplicable             = "01 - No aplica"
  show ExportDefinitive                = "02 - Definitiva"
  show ExportTemporary                 = "03 - Temporal"
  show ExportDefinitiveWithoutDisposal = "04 - Definitiva sin enajenación"

instance Type Export where
  parseExpr "01" = Right ExportNotApplicable
  parseExpr "02" = Right ExportDefinitive
  parseExpr "03" = Right ExportTemporary
  parseExpr "04" = Right ExportDefinitiveWithoutDisposal
  parseExpr _    = Left NotInCatalog

  render ExportNotApplicable             = "01"
  render ExportDefinitive                = "02"
  render ExportTemporary                 = "03"
  render ExportDefinitiveWithoutDisposal = "04"
