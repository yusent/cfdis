module CFDI.Types.Use where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text       (pack)

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
  deriving (Bounded, Enum, Eq)

instance Chainable Use where
  chain = pack . render

instance Show Use where
  show GoodsAcquisition_         = "G01 - Adquisición de mercancias"
  show ReturnsDiscountsOrBonuses = "G02 - Devoluciones, descuentos o \
                                   \bonificaciones"
  show GeneralExpenses           = "G03 - Gastos en general"
  show Constructions             = "I01 - Construcciones"
  show Furniture                 = "I02 - Mobilario y equipo de oficina por \
                                   \inversiones"
  show TransportEquipment        = "I03 - Equipo de transporte"
  show ComputerEquipment         = "I04 - Equipo de computo y accesorios"
  show Tooling                   = "I05 - Dados, troqueles, moldes, matrices y \
                                   \herramental"
  show PhoneComunications        = "I06 - Comunicaciones telefónicas"
  show SatelliteComunications    = "I07 - Comunicaciones satelitales"
  show OtherMachinery            = "I08 - Otra maquinaria y equipo"
  show MedicalFees               = "D01 - Honorarios médicos, dentales y \
                                   \gastos hospitalarios."
  show MedicalExpenses           = "D02 - Gastos médicos por incapacidad o \
                                   \discapacidad"
  show FuneralExpenses           = "D03 - Gastos funerales."
  show Donations                 = "D04 - Donativos."
  show PaidInterests             = "D05 - Intereses reales efectivamente \
                                   \pagados por créditos hipotecarios (casa \
                                   \habitación)."
  show VoluntaryContributions    = "D06 - Aportaciones voluntarias al SAR."
  show InsurancePremiums         = "D07 - Primas por seguros de gastos médicos."
  show SchoolTransportation      = "D08 - Gastos de transportación escolar \
                                   \obligatoria."
  show DepositsInSavingsAccounts = "D09 - Depósitos en cuentas para el ahorro, \
                                   \primas que tengan como base planes de \
                                   \pensiones."
  show TuitionFees               = "D10 - Pagos por servicios educativos \
                                   \(colegiaturas)"
  show ToBeDefined_              = "P01 - Por definir"

instance Type Use where
  parseExpr "G01" = Right GoodsAcquisition_
  parseExpr "G02" = Right ReturnsDiscountsOrBonuses
  parseExpr "G03" = Right GeneralExpenses
  parseExpr "I01" = Right Constructions
  parseExpr "I02" = Right Furniture
  parseExpr "I03" = Right TransportEquipment
  parseExpr "I04" = Right ComputerEquipment
  parseExpr "I05" = Right Tooling
  parseExpr "I06" = Right PhoneComunications
  parseExpr "I07" = Right SatelliteComunications
  parseExpr "I08" = Right OtherMachinery
  parseExpr "D01" = Right MedicalFees
  parseExpr "D02" = Right MedicalExpenses
  parseExpr "D03" = Right FuneralExpenses
  parseExpr "D04" = Right Donations
  parseExpr "D05" = Right PaidInterests
  parseExpr "D06" = Right VoluntaryContributions
  parseExpr "D07" = Right InsurancePremiums
  parseExpr "D08" = Right SchoolTransportation
  parseExpr "D09" = Right DepositsInSavingsAccounts
  parseExpr "D10" = Right TuitionFees
  parseExpr "P01" = Right ToBeDefined_
  parseExpr _     = Left NotInCatalog

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
