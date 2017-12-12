module CFDI.Types.WayToPay where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text       (pack)

data WayToPay
  = Cash
  | NominalCheck
  | ElectronicTransfer
  | CreditCard
  | ElectronicPurse
  | ElectronicCash
  | PantryCoupons
  | PaymentIn
  | Subrogation
  | Consignment
  | Condonation
  | Compensation
  | Novation
  | Confusion
  | DebtReferral
  | PrescriptionOrExpiration
  | ToTheSatisfactionOfTheCreditor
  | DebitCard
  | ServiceCard
  | AdvancesApplication
  | PaymentIntermediary
  | ToBeDefined
  deriving (Bounded, Enum, Eq)

instance Chainable WayToPay where
  chain = pack . render

instance Show WayToPay where
  show Cash                           = "01 - Efectivo"
  show NominalCheck                   = "02 - Cheque nominativo"
  show ElectronicTransfer             = "03 - Transferencia electrónica de \
                                        \fondos"
  show CreditCard                     = "04 - Tarjeta de crédito"
  show ElectronicPurse                = "05 - Monedero electrónico"
  show ElectronicCash                 = "06 - Dinero electrónico"
  show PantryCoupons                  = "08 - Vales de despensa"
  show PaymentIn                      = "12 - Dación en pago"
  show Subrogation                    = "13 - Pago por subrogación"
  show Consignment                    = "14 - Pago por consignación"
  show Condonation                    = "15 - Condonación"
  show Compensation                   = "17 - Compensación"
  show Novation                       = "23 - Novación"
  show Confusion                      = "24 - Confusión"
  show DebtReferral                   = "25 - Remisión de deuda"
  show PrescriptionOrExpiration       = "26 - Prescripción o caducidad"
  show ToTheSatisfactionOfTheCreditor = "27 - A satisfacción del acreedor"
  show DebitCard                      = "28 - Tarjeta de débito"
  show ServiceCard                    = "29 - Tarjeta de servicios"
  show AdvancesApplication            = "30 - Aplicación de anticipos"
  show PaymentIntermediary            = "31 - Intermediario pagos"
  show ToBeDefined                    = "99 - Por definir"

instance Type WayToPay where
  parseExpr "01" = Right Cash
  parseExpr "02" = Right NominalCheck
  parseExpr "03" = Right ElectronicTransfer
  parseExpr "04" = Right CreditCard
  parseExpr "05" = Right ElectronicPurse
  parseExpr "06" = Right ElectronicCash
  parseExpr "08" = Right PantryCoupons
  parseExpr "12" = Right PaymentIn
  parseExpr "13" = Right Subrogation
  parseExpr "14" = Right Consignment
  parseExpr "15" = Right Condonation
  parseExpr "17" = Right Compensation
  parseExpr "23" = Right Novation
  parseExpr "24" = Right Confusion
  parseExpr "25" = Right DebtReferral
  parseExpr "26" = Right PrescriptionOrExpiration
  parseExpr "27" = Right ToTheSatisfactionOfTheCreditor
  parseExpr "28" = Right DebitCard
  parseExpr "29" = Right ServiceCard
  parseExpr "30" = Right AdvancesApplication
  parseExpr "31" = Right PaymentIntermediary
  parseExpr "99" = Right ToBeDefined
  parseExpr _    = Left NotInCatalog

  render Cash                           = "01"
  render NominalCheck                   = "02"
  render ElectronicTransfer             = "03"
  render CreditCard                     = "04"
  render ElectronicPurse                = "05"
  render ElectronicCash                 = "06"
  render PantryCoupons                  = "08"
  render PaymentIn                      = "12"
  render Subrogation                    = "13"
  render Consignment                    = "14"
  render Condonation                    = "15"
  render Compensation                   = "17"
  render Novation                       = "23"
  render Confusion                      = "24"
  render DebtReferral                   = "25"
  render PrescriptionOrExpiration       = "26"
  render ToTheSatisfactionOfTheCreditor = "27"
  render DebitCard                      = "28"
  render ServiceCard                    = "29"
  render AdvancesApplication            = "30"
  render PaymentIntermediary            = "31"
  render ToBeDefined                    = "99"
