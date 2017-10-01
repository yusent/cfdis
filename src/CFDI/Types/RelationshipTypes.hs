module CFDI.Types.RelationshipTypes where

import CFDI.Types.Catalog

data RelationshipType
  = CreditNote
  | DebitNote
  | ReturnedGoods
  | Substitution
  | PrevInvoicedTransfer
  | PrevTransferedInvoice
  | AdvanceApplication
  deriving (Eq, Show)

instance Catalog RelationshipType where
  fromCode "01" = Just CreditNote
  fromCode "02" = Just DebitNote
  fromCode "03" = Just ReturnedGoods
  fromCode "04" = Just Substitution
  fromCode "05" = Just PrevInvoicedTransfer
  fromCode "06" = Just PrevTransferedInvoice
  fromCode "07" = Just AdvanceApplication
  fromCode _    = Nothing

  toCode CreditNote            = "01"
  toCode DebitNote             = "02"
  toCode ReturnedGoods         = "03"
  toCode Substitution          = "04"
  toCode PrevInvoicedTransfer  = "05"
  toCode PrevTransferedInvoice = "06"
  toCode AdvanceApplication    = "07"
