{-# LANGUAGE OverloadedStrings #-}

module CFDI.Catalogs.WaysToPay where

import CFDI.Catalog

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
