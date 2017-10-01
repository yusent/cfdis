module CFDI.Types.TaxRegime where

import CFDI.Types.Type

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
  deriving (Eq, Show)

instance Type TaxRegime where
  parse "600" = Right GeneralForPeople
  parse "602" = Right NonProfitCompany
  parse "605" = Right WagesAndSalaries
  parse "606" = Right Leasing
  parse "607" = Right GoodsAcquisition
  parse "608" = Right OtherIncomes
  parse "609" = Right Consolidation
  parse "610" = Right ForeignResidents
  parse "611" = Right DividendIncomes
  parse "612" = Right PeopleWithBusinessActivities
  parse "614" = Right InterestsIncomes
  parse "615" = Right RevenueForPrizes
  parse "616" = Right NoTaxObligations
  parse "620" = Right CooperativeSocieties
  parse "621" = Right FiscalIncorporation
  parse "622" = Right AgriculturalActivities
  parse "623" = Right OptionalForSocialGroups
  parse "624" = Right Coordinated
  parse "628" = Right Hydrocarbons
  parse "629" = Right MultinationalCompanies
  parse "630" = Right SharesAlienation
  parse _     = Left NotInCatalog

  render GeneralForPeople             = "600"
  render NonProfitCompany             = "602"
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
