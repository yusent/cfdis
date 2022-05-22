module CFDI.Types.TaxObject where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text (pack)

data TaxObject
  = NotSubjectToTax
  | SubjectToTax
  | NoObligationToBreakDown
  deriving (Bounded, Enum, Eq)

instance Chainable TaxObject where
  chain = pack . render

instance Show TaxObject where
  show NotSubjectToTax         = "01 - No objeto de impuesto"
  show SubjectToTax            = "02 - Sí objeto de impuesto"
  show NoObligationToBreakDown = "03 - Sí objeto de impuesto y no obligado al \
                                 \desglose"

instance Type TaxObject where
  parseExpr "01" = Right NotSubjectToTax
  parseExpr "02" = Right SubjectToTax
  parseExpr "03" = Right NoObligationToBreakDown
  parseExpr _     = Left NotInCatalog

  render NotSubjectToTax         = "01"
  render SubjectToTax            = "02"
  render NoObligationToBreakDown = "03"
