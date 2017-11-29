module CFDI.Types.TaxRegime where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text       (pack)

data TaxRegime
  = GeneralForPeople
  | NonProfitCompany
  | WagesAndSalaries
  | Leasing
  | GoodsAcquisition
  | OtherIncomes
  | Consolidation
  | ForeignResidents
  | DividendIncomes
  | PeopleWithBusinessActivities
  | InterestsIncomes
  | RevenueForPrizes
  | NoTaxObligations
  | CooperativeSocieties
  | FiscalIncorporation
  | AgriculturalActivities
  | OptionalForSocialGroups
  | Coordinated
  | Hydrocarbons
  | MultinationalCompanies
  | SharesAlienation
  deriving (Bounded, Enum, Eq)

instance Chainable TaxRegime where
  chain = pack . render

instance Type TaxRegime where
  parseExpr "601" = Right GeneralForPeople
  parseExpr "603" = Right NonProfitCompany
  parseExpr "605" = Right WagesAndSalaries
  parseExpr "606" = Right Leasing
  parseExpr "607" = Right GoodsAcquisition
  parseExpr "608" = Right OtherIncomes
  parseExpr "609" = Right Consolidation
  parseExpr "610" = Right ForeignResidents
  parseExpr "611" = Right DividendIncomes
  parseExpr "612" = Right PeopleWithBusinessActivities
  parseExpr "614" = Right InterestsIncomes
  parseExpr "615" = Right RevenueForPrizes
  parseExpr "616" = Right NoTaxObligations
  parseExpr "620" = Right CooperativeSocieties
  parseExpr "621" = Right FiscalIncorporation
  parseExpr "622" = Right AgriculturalActivities
  parseExpr "623" = Right OptionalForSocialGroups
  parseExpr "624" = Right Coordinated
  parseExpr "628" = Right Hydrocarbons
  parseExpr "629" = Right MultinationalCompanies
  parseExpr "630" = Right SharesAlienation
  parseExpr _     = Left NotInCatalog

  render GeneralForPeople             = "601"
  render NonProfitCompany             = "603"
  render WagesAndSalaries             = "605"
  render Leasing                      = "606"
  render GoodsAcquisition             = "607"
  render OtherIncomes                 = "608"
  render Consolidation                = "609"
  render ForeignResidents             = "610"
  render DividendIncomes              = "611"
  render PeopleWithBusinessActivities = "612"
  render InterestsIncomes             = "614"
  render RevenueForPrizes             = "615"
  render NoTaxObligations             = "616"
  render CooperativeSocieties         = "620"
  render FiscalIncorporation          = "621"
  render AgriculturalActivities       = "622"
  render OptionalForSocialGroups      = "623"
  render Coordinated                  = "624"
  render Hydrocarbons                 = "628"
  render MultinationalCompanies       = "629"
  render SharesAlienation             = "630"

instance Show TaxRegime where
  show GeneralForPeople             = "General de Ley Personas Morales"
  show NonProfitCompany             = "Personas Morales con Fines no Lucrativos"
  show WagesAndSalaries             = "Sueldos y Salarios e Ingresos \
                                      \Asimilados a Salarios"
  show Leasing                      = "Arrendamiento"
  show GoodsAcquisition             = "Régimen de Enajenación o Adquisición de \
                                      \Bienes"
  show OtherIncomes                 = "Demás ingresos"
  show Consolidation                = "Consolidación"
  show ForeignResidents             = "Residentes en el Extranjero sin \
                                      \Establecimiento Permanente en México"
  show DividendIncomes              = "Ingresos por Dividendos \
                                      \(socios y accionistas)"
  show PeopleWithBusinessActivities = "Personas Físicas con Actividades \
                                      \Empresariales y Profesionales"
  show InterestsIncomes             = "Ingresos por intereses"
  show RevenueForPrizes             = "Régimen de los ingresos por obtención \
                                      \de premios"
  show NoTaxObligations             = "Sin obligaciones fiscales"
  show CooperativeSocieties         = "Sociedades Cooperativas de Producción \
                                      \que optan por diferir sus ingresos"
  show FiscalIncorporation          = "Incorporación Fiscal"
  show AgriculturalActivities       = "Actividades Agrícolas, Ganaderas, \
                                      \Silvícolas y Pesqueras"
  show OptionalForSocialGroups      = "Opcional para Grupos de Sociedades"
  show Coordinated                  = "Coordinados"
  show Hydrocarbons                 = "Hidrocarburos"
  show MultinationalCompanies       = "De los Regímenes Fiscales Preferentes y \
                                      \de las Empresas Multinacionales"
  show SharesAlienation             = "Enajenación de acciones en bolsa de \
                                      \valores"
