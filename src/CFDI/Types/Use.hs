module CFDI.Types.Use where

import CFDI.Types.Type

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

instance Type Use where
  parse "G01" = Right GoodsAcquisition_
  parse "G02" = Right ReturnsDiscountsOrBonuses
  parse "G03" = Right GeneralExpenses
  parse "I01" = Right Constructions
  parse "I02" = Right Furniture
  parse "I03" = Right TransportEquipment
  parse "I04" = Right ComputerEquipment
  parse "I05" = Right Tooling
  parse "I06" = Right PhoneComunications
  parse "I07" = Right SatelliteComunications
  parse "I08" = Right OtherMachinery
  parse "D01" = Right MedicalFees
  parse "D02" = Right MedicalExpenses
  parse "D03" = Right FuneralExpenses
  parse "D04" = Right Donations
  parse "D05" = Right PaidInterests
  parse "D06" = Right VoluntaryContributions
  parse "D07" = Right InsurancePremiums
  parse "D08" = Right SchoolTransportation
  parse "D09" = Right DepositsInSavingsAccounts
  parse "D10" = Right TuitionFees
  parse "P01" = Right ToBeDefined_
  parse _     = Left NotInCatalog

  render GoodsAcquisition_         = "G01"
  render ReturnsDiscountsOrBonuses = "G02"
  render GeneralExpenses           = "G03"
  render Constructions             = "I01"
  render Furniture                 = "I02"
  render TransportEquipment        = "I03"
  render ComputerEquipment         = "I04"
  render Tooling                   = "I05"
  render PhoneComunications        = "I06"
  render SatelliteComunications    = "I07"
  render OtherMachinery            = "I08"
  render MedicalFees               = "D01"
  render MedicalExpenses           = "D02"
  render FuneralExpenses           = "D03"
  render Donations                 = "D04"
  render PaidInterests             = "D05"
  render VoluntaryContributions    = "D06"
  render InsurancePremiums         = "D07"
  render SchoolTransportation      = "D08"
  render DepositsInSavingsAccounts = "D09"
  render TuitionFees               = "D10"
  render ToBeDefined_              = "P01"
