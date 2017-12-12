module CFDI.Types.RelationshipType where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text       (pack)

data RelationshipType
  = CreditNote
  | DebitNote
  | ReturnedGoods
  | Substitution
  | PrevInvoicedTransfer
  | PrevTransferedInvoice
  | AdvanceApplication
  | PartialPaymentInvoice
  | DeferredPaymentInvoice
  deriving (Bounded, Enum, Eq)

instance Chainable RelationshipType where
  chain = pack . render

instance Show RelationshipType where
  show CreditNote             = "Nota de crédito de los documentos relacionados"
  show DebitNote              = "Nota de débito de los documentos relacionados"
  show ReturnedGoods          = "Devolución de mercancía sobre facturas o \
                                \traslados previos"
  show Substitution           = "Sustitución de los CFDI previos"
  show PrevInvoicedTransfer   = "Traslados de mercancias facturados previamente"
  show PrevTransferedInvoice  = "Factura generada por los traslados previos"
  show AdvanceApplication     = "CFDI por aplicación de anticipo"
  show PartialPaymentInvoice  = "Factura generada por pagos en parcialidades"
  show DeferredPaymentInvoice = "Factura generada por pagos diferidos"

instance Type RelationshipType where
  parseExpr "01" = Right CreditNote
  parseExpr "02" = Right DebitNote
  parseExpr "03" = Right ReturnedGoods
  parseExpr "04" = Right Substitution
  parseExpr "05" = Right PrevInvoicedTransfer
  parseExpr "06" = Right PrevTransferedInvoice
  parseExpr "07" = Right AdvanceApplication
  parseExpr "08" = Right PartialPaymentInvoice
  parseExpr "09" = Right DeferredPaymentInvoice
  parseExpr _    = Left NotInCatalog

  render CreditNote             = "01"
  render DebitNote              = "02"
  render ReturnedGoods          = "03"
  render Substitution           = "04"
  render PrevInvoicedTransfer   = "05"
  render PrevTransferedInvoice  = "06"
  render AdvanceApplication     = "07"
  render PartialPaymentInvoice  = "08"
  render DeferredPaymentInvoice = "09"
