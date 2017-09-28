{-# LANGUAGE OverloadedStrings #-}

module CFDI.Catalogs where

import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)

class Catalog c where
  fromCode :: Text -> Maybe c

  toCode :: c -> Text

data CfdiType
  = Income
  | Outcome
  | Transfer
  | Paysheet
  | Payment
  deriving (Eq, Show)

instance Catalog CfdiType where
  fromCode "I" = Just Income
  fromCode "E" = Just Outcome
  fromCode "T" = Just Transfer
  fromCode "N" = Just Paysheet
  fromCode "P" = Just Payment
  fromCode _   = Nothing

  toCode Income   = "I"
  toCode Outcome  = "E"
  toCode Transfer = "T"
  toCode Paysheet = "N"
  toCode Payment  = "P"

data Currency
  = AED
  | AFN
  | ALL
  | AMD
  | ANG
  | AOA
  | ARS
  | AUD
  | AWG
  | AZN
  | BAM
  | BBD
  | BDT
  | BGN
  | BHD
  | BIF
  | BMD
  | BND
  | BOB
  | BOV
  | BRL
  | BSD
  | BTN
  | BWP
  | BYN
  | BZD
  | CAD
  | CDF
  | CHE
  | CHF
  | CHW
  | CLF
  | CLP
  | CNY
  | COP
  | COU
  | CRC
  | CUC
  | CUP
  | CVE
  | CZK
  | DJF
  | DKK
  | DOP
  | DZD
  | EGP
  | ERN
  | ETB
  | EUR
  | FJD
  | FKP
  | GBP
  | GEL
  | GHS
  | GIP
  | GMD
  | GNF
  | GTQ
  | GYD
  | HKD
  | HNL
  | HRK
  | HTG
  | HUF
  | IDR
  | ILS
  | INR
  | IQD
  | IRR
  | ISK
  | JMD
  | JOD
  | JPY
  | KES
  | KGS
  | KHR
  | KMF
  | KPW
  | KRW
  | KWD
  | KYD
  | KZT
  | LAK
  | LBP
  | LKR
  | LRD
  | LSL
  | LYD
  | MAD
  | MDL
  | MGA
  | MKD
  | MMK
  | MNT
  | MOP
  | MRO
  | MUR
  | MVR
  | MWK
  | MXN
  | MXV
  | MYR
  | MZN
  | NAD
  | NGN
  | NIO
  | NOK
  | NPR
  | NZD
  | OMR
  | PAB
  | PEN
  | PGK
  | PHP
  | PKR
  | PLN
  | PYG
  | QAR
  | RON
  | RSD
  | RUB
  | RWF
  | SAR
  | SBD
  | SCR
  | SDG
  | SEK
  | SGD
  | SHP
  | SLL
  | SOS
  | SRD
  | SSP
  | STD
  | SVC
  | SYP
  | SZL
  | THB
  | TJS
  | TMT
  | TND
  | TOP
  | TRY
  | TTD
  | TWD
  | TZS
  | UAH
  | UGX
  | USD
  | USN
  | UYI
  | UYU
  | UZS
  | VEF
  | VND
  | VUV
  | WST
  | XAF
  | XAG
  | XAU
  | XBA
  | XBB
  | XBC
  | XBD
  | XCD
  | XDR
  | XOF
  | XPD
  | XPF
  | XPT
  | XSU
  | XTS
  | XUA
  | XXX
  | YER
  | ZAR
  | ZMW
  | ZWL
  deriving (Eq, Read, Show)

instance Catalog Currency where
  fromCode = readMaybe . unpack

  toCode = pack . show

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

instance Catalog WayToPay where
  fromCode "01" = Just Cash
  fromCode "02" = Just NominalCheck
  fromCode "03" = Just ElectronicTransfer
  fromCode "04" = Just CreditCard
  fromCode "05" = Just ElectronicPurse
  fromCode "06" = Just ElectronicCash
  fromCode "08" = Just PantryCoupons
  fromCode "12" = Just PaymentIn
  fromCode "13" = Just Subrogation
  fromCode "14" = Just Consignment
  fromCode "15" = Just Condonation
  fromCode "17" = Just Compensation
  fromCode "23" = Just Novation
  fromCode "24" = Just Confusion
  fromCode "25" = Just DebtReferral
  fromCode "26" = Just PrescriptionOrExpiration
  fromCode "27" = Just ToTheSatisfactionOfTheCreditor
  fromCode "28" = Just DebitCard
  fromCode "29" = Just ServiceCard
  fromCode "30" = Just AdvancesApplication
  fromCode "99" = Just ToBeDefined
  fromCode _    = Nothing

  toCode Cash                           = "01"
  toCode NominalCheck                   = "02"
  toCode ElectronicTransfer             = "03"
  toCode CreditCard                     = "04"
  toCode ElectronicPurse                = "05"
  toCode ElectronicCash                 = "06"
  toCode PantryCoupons                  = "08"
  toCode PaymentIn                      = "12"
  toCode Subrogation                    = "13"
  toCode Consignment                    = "14"
  toCode Condonation                    = "15"
  toCode Compensation                   = "17"
  toCode Novation                       = "23"
  toCode Confusion                      = "24"
  toCode DebtReferral                   = "25"
  toCode PrescriptionOrExpiration       = "26"
  toCode ToTheSatisfactionOfTheCreditor = "27"
  toCode DebitCard                      = "28"
  toCode ServiceCard                    = "29"
  toCode AdvancesApplication            = "30"
  toCode ToBeDefined                    = "99"
