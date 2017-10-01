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
  parse "01" = Right CreditNote
  parse "02" = Right DebitNote
  parse "03" = Right ReturnedGoods
  parse "04" = Right Substitution
  parse "05" = Right PrevInvoicedTransfer
  parse "06" = Right PrevTransferedInvoice
  parse "07" = Right AdvanceApplication
  parse _    = Left NotInCatalog

  render CreditNote            = "01"
  render DebitNote             = "02"
  render ReturnedGoods         = "03"
  render Substitution          = "04"
  render PrevInvoicedTransfer  = "05"
  render PrevTransferedInvoice = "06"
  render AdvanceApplication    = "07"
