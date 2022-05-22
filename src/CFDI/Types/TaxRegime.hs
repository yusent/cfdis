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
  | TechnologicalPlatforms
  | SimplifiedTrust
  | Hydrocarbons
  | MultinationalCompanies
  | SharesAlienation
  deriving (Bounded, Enum, Eq)

instance Chainable TaxRegime where
  chain = pack . render

instance Show TaxRegime where
  show GeneralForPeople             = "601 - General de Ley Personas Morales"
  show NonProfitCompany             = "603 - Personas Morales con Fines no \
                                      \Lucrativos"
  show WagesAndSalaries             = "605 - Sueldos y Salarios e Ingresos \
                                      \Asimilados a Salarios"
  show Leasing                      = "606 - Arrendamiento"
  show GoodsAcquisition             = "607 - Régimen de Enajenación o \
                                      \Adquisición de Bienes"
  show OtherIncomes                 = "608 - Demás ingresos"
  show Consolidation                = "609 - Consolidación"
  show ForeignResidents             = "610 - Residentes en el Extranjero sin \
                                      \Establecimiento Permanente en México"
  show DividendIncomes              = "611 - Ingresos por Dividendos (socios y \
                                      \accionistas)"
  show PeopleWithBusinessActivities = "612 - Personas Físicas con Actividades \
                                      \Empresariales y Profesionales"
  show InterestsIncomes             = "614 - Ingresos por intereses"
  show RevenueForPrizes             = "615 - Régimen de los ingresos por \
                                      \obtención de premios"
  show NoTaxObligations             = "616 - Sin obligaciones fiscales"
  show CooperativeSocieties         = "620 - Sociedades Cooperativas de \
                                      \Producción que optan por diferir sus \
                                      \ingresos"
  show FiscalIncorporation          = "621 - Incorporación Fiscal"
  show AgriculturalActivities       = "622 - Actividades Agrícolas, Ganaderas, \
                                      \Silvícolas y Pesqueras"
  show OptionalForSocialGroups      = "623 - Opcional para Grupos de Sociedades"
  show Coordinated                  = "624 - Coordinados"
  show TechnologicalPlatforms       = "625 - Régimen de las Actividades \
                                      \Empresariales con ingresos a través de \
                                      \Plataformas Tecnológicas"
  show SimplifiedTrust              = "626 - Régimen Simplificado de Confianza"
  show Hydrocarbons                 = "628 - Hidrocarburos"
  show MultinationalCompanies       = "629 - De los Regímenes Fiscales \
                                      \Preferentes y de las Empresas \
                                      \Multinacionales"
  show SharesAlienation             = "630 - Enajenación de acciones en bolsa \
                                      \de valores"

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
  parseExpr "625" = Right TechnologicalPlatforms
  parseExpr "626" = Right SimplifiedTrust
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
  render TechnologicalPlatforms       = "625"
  render SimplifiedTrust              = "626"
  render Hydrocarbons                 = "628"
  render MultinationalCompanies       = "629"
  render SharesAlienation             = "630"
