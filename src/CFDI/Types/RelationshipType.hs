module CFDI.Types.RelationshipType where

import CFDI.Types.Type

data RelationshipType
  = CreditNote
  | DebitNote
  | ReturnedGoods
  | Substitution
  | PrevInvoicedTransfer
  | PrevTransferedInvoice
  | AdvanceApplication
  deriving (Eq, Show)

instance Type RelationshipType where
  parseExpr "01" = Right CreditNote
  parseExpr "02" = Right DebitNote
  parseExpr "03" = Right ReturnedGoods
  parseExpr "04" = Right Substitution
  parseExpr "05" = Right PrevInvoicedTransfer
  parseExpr "06" = Right PrevTransferedInvoice
  parseExpr "07" = Right AdvanceApplication
  parseExpr _    = Left NotInCatalog

  render CreditNote            = "01"
  render DebitNote             = "02"
  render ReturnedGoods         = "03"
  render Substitution          = "04"
  render PrevInvoicedTransfer  = "05"
  render PrevTransferedInvoice = "06"
  render AdvanceApplication    = "07"
