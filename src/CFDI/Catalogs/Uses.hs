module CFDI.Catalogs.Uses where

import CFDI.Catalog

data Use
  = GoodsAcquisition_
  | ReturnsDiscountsOrBonuses
  | GeneralExpenses
  | Constructions
  | Furniture
  | TransportEquipment
  | ComputerEquipment
  | Tooling
  | PhoneComunications
  | SatelliteComunications
  | OtherMachinery
  | MedicalFees
  | MedicalExpenses
  | FuneralExpenses
  | Donations
  | PaidInterests
  | VoluntaryContributions
  | InsurancePremiums
  | SchoolTransportation
  | DepositsInSavingsAccounts
  | TuitionFees
  | ToBeDefined_
  deriving (Eq, Show)

instance Catalog Use where
  fromCode "G01" = Just GoodsAcquisition_
  fromCode "G02" = Just ReturnsDiscountsOrBonuses
  fromCode "G03" = Just GeneralExpenses
  fromCode "I01" = Just Constructions
  fromCode "I02" = Just Furniture
  fromCode "I03" = Just TransportEquipment
  fromCode "I04" = Just ComputerEquipment
  fromCode "I05" = Just Tooling
  fromCode "I06" = Just PhoneComunications
  fromCode "I07" = Just SatelliteComunications
  fromCode "I08" = Just OtherMachinery
  fromCode "D01" = Just MedicalFees
  fromCode "D02" = Just MedicalExpenses
  fromCode "D03" = Just FuneralExpenses
  fromCode "D04" = Just Donations
  fromCode "D05" = Just PaidInterests
  fromCode "D06" = Just VoluntaryContributions
  fromCode "D07" = Just InsurancePremiums
  fromCode "D08" = Just SchoolTransportation
  fromCode "D09" = Just DepositsInSavingsAccounts
  fromCode "D10" = Just TuitionFees
  fromCode "P01" = Just ToBeDefined_
  fromCode _     = Nothing

  toCode GoodsAcquisition_         = "G01"
  toCode ReturnsDiscountsOrBonuses = "G02"
  toCode GeneralExpenses           = "G03"
  toCode Constructions             = "I01"
  toCode Furniture                 = "I02"
  toCode TransportEquipment        = "I03"
  toCode ComputerEquipment         = "I04"
  toCode Tooling                   = "I05"
  toCode PhoneComunications        = "I06"
  toCode SatelliteComunications    = "I07"
  toCode OtherMachinery            = "I08"
  toCode MedicalFees               = "D01"
  toCode MedicalExpenses           = "D02"
  toCode FuneralExpenses           = "D03"
  toCode Donations                 = "D04"
  toCode PaidInterests             = "D05"
  toCode VoluntaryContributions    = "D06"
  toCode InsurancePremiums         = "D07"
  toCode SchoolTransportation      = "D08"
  toCode DepositsInSavingsAccounts = "D09"
  toCode TuitionFees               = "D10"
  toCode ToBeDefined_              = "P01"
