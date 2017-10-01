module CFDI.Catalogs.TaxRegimes where

import CFDI.Catalog

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

instance Catalog TaxRegime where
  fromCode "600" = Just GeneralForPeople
  fromCode "602" = Just NonProfitCompany
  fromCode "605" = Just WagesAndSalaries
  fromCode "606" = Just Leasing
  fromCode "607" = Just GoodsAcquisition
  fromCode "608" = Just OtherIncomes
  fromCode "609" = Just Consolidation
  fromCode "610" = Just ForeignResidents
  fromCode "611" = Just DividendIncomes
  fromCode "612" = Just PeopleWithBusinessActivities
  fromCode "614" = Just InterestsIncomes
  fromCode "615" = Just RevenueForPrizes
  fromCode "616" = Just NoTaxObligations
  fromCode "620" = Just CooperativeSocieties
  fromCode "621" = Just FiscalIncorporation
  fromCode "622" = Just AgriculturalActivities
  fromCode "623" = Just OptionalForSocialGroups
  fromCode "624" = Just Coordinated
  fromCode "628" = Just Hydrocarbons
  fromCode "629" = Just MultinationalCompanies
  fromCode "630" = Just SharesAlienation
  fromCode _     = Nothing

  toCode GeneralForPeople             = "600"
  toCode NonProfitCompany             = "602"
  toCode WagesAndSalaries             = "605"
  toCode Leasing                      = "606"
  toCode GoodsAcquisition             = "607"
  toCode OtherIncomes                 = "608"
  toCode Consolidation                = "609"
  toCode ForeignResidents             = "610"
  toCode DividendIncomes              = "611"
  toCode PeopleWithBusinessActivities = "612"
  toCode InterestsIncomes             = "614"
  toCode RevenueForPrizes             = "615"
  toCode NoTaxObligations             = "616"
  toCode CooperativeSocieties         = "620"
  toCode FiscalIncorporation          = "621"
  toCode AgriculturalActivities       = "622"
  toCode OptionalForSocialGroups      = "623"
  toCode Coordinated                  = "624"
  toCode Hydrocarbons                 = "628"
  toCode MultinationalCompanies       = "629"
  toCode SharesAlienation             = "630"
