module CFDI.Types.WayToPay where

import CFDI.Types.Type

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
  | ToBeDefined
  deriving (Eq, Show)

instance Type WayToPay where
  parse "01" = Right Cash
  parse "02" = Right NominalCheck
  parse "03" = Right ElectronicTransfer
  parse "04" = Right CreditCard
  parse "05" = Right ElectronicPurse
  parse "06" = Right ElectronicCash
  parse "08" = Right PantryCoupons
  parse "12" = Right PaymentIn
  parse "13" = Right Subrogation
  parse "14" = Right Consignment
  parse "15" = Right Condonation
  parse "17" = Right Compensation
  parse "23" = Right Novation
  parse "24" = Right Confusion
  parse "25" = Right DebtReferral
  parse "26" = Right PrescriptionOrExpiration
  parse "27" = Right ToTheSatisfactionOfTheCreditor
  parse "28" = Right DebitCard
  parse "29" = Right ServiceCard
  parse "30" = Right AdvancesApplication
  parse "99" = Right ToBeDefined
  parse _    = Left NotInCatalog

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
  render ToBeDefined                    = "99"
