module CFDI.Types.TaxObject where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text (pack)

data TaxObject
  = NotSubjectToTax
  | SubjectToTax
  | NoObligationToBreakDown
  deriving (Eq, Read, Show)

instance Chainable TaxObject where
  chain = pack . render

instance Type TaxObject where
  parseExpr "01" = Right NotSubjectToTax
  parseExpr "02" = Right SubjectToTax
  parseExpr "03" = Right NoObligationToBreakDown
  parseExpr _     = Left NotInCatalog

  render NotSubjectToTax         = "01"
  render SubjectToTax            = "02"
  render NoObligationToBreakDown = "03"
