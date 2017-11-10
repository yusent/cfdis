module CFDI.Types.Currency where

import CFDI.Chainable
import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Data.Text          (pack)
import Text.Read          (readMaybe)

data Currency
  = CUR_AED
  | CUR_AFN
  | CUR_ALL
  | CUR_AMD
  | CUR_ANG
  | CUR_AOA
  | CUR_ARS
  | CUR_AUD
  | CUR_AWG
  | CUR_AZN
  | CUR_BAM
  | CUR_BBD
  | CUR_BDT
  | CUR_BGN
  | CUR_BHD
  | CUR_BIF
  | CUR_BMD
  | CUR_BND
  | CUR_BOB
  | CUR_BOV
  | CUR_BRL
  | CUR_BSD
  | CUR_BTN
  | CUR_BWP
  | CUR_BYN
  | CUR_BZD
  | CUR_CAD
  | CUR_CDF
  | CUR_CHE
  | CUR_CHF
  | CUR_CHW
  | CUR_CLF
  | CUR_CLP
  | CUR_CNY
  | CUR_COP
  | CUR_COU
  | CUR_CRC
  | CUR_CUC
  | CUR_CUP
  | CUR_CVE
  | CUR_CZK
  | CUR_DJF
  | CUR_DKK
  | CUR_DOP
  | CUR_DZD
  | CUR_EGP
  | CUR_ERN
  | CUR_ETB
  | CUR_EUR
  | CUR_FJD
  | CUR_FKP
  | CUR_GBP
  | CUR_GEL
  | CUR_GHS
  | CUR_GIP
  | CUR_GMD
  | CUR_GNF
  | CUR_GTQ
  | CUR_GYD
  | CUR_HKD
  | CUR_HNL
  | CUR_HRK
  | CUR_HTG
  | CUR_HUF
  | CUR_IDR
  | CUR_ILS
  | CUR_INR
  | CUR_IQD
  | CUR_IRR
  | CUR_ISK
  | CUR_JMD
  | CUR_JOD
  | CUR_JPY
  | CUR_KES
  | CUR_KGS
  | CUR_KHR
  | CUR_KMF
  | CUR_KPW
  | CUR_KRW
  | CUR_KWD
  | CUR_KYD
  | CUR_KZT
  | CUR_LAK
  | CUR_LBP
  | CUR_LKR
  | CUR_LRD
  | CUR_LSL
  | CUR_LYD
  | CUR_MAD
  | CUR_MDL
  | CUR_MGA
  | CUR_MKD
  | CUR_MMK
  | CUR_MNT
  | CUR_MOP
  | CUR_MRO
  | CUR_MUR
  | CUR_MVR
  | CUR_MWK
  | CUR_MXN
  | CUR_MXV
  | CUR_MYR
  | CUR_MZN
  | CUR_NAD
  | CUR_NGN
  | CUR_NIO
  | CUR_NOK
  | CUR_NPR
  | CUR_NZD
  | CUR_OMR
  | CUR_PAB
  | CUR_PEN
  | CUR_PGK
  | CUR_PHP
  | CUR_PKR
  | CUR_PLN
  | CUR_PYG
  | CUR_QAR
  | CUR_RON
  | CUR_RSD
  | CUR_RUB
  | CUR_RWF
  | CUR_SAR
  | CUR_SBD
  | CUR_SCR
  | CUR_SDG
  | CUR_SEK
  | CUR_SGD
  | CUR_SHP
  | CUR_SLL
  | CUR_SOS
  | CUR_SRD
  | CUR_SSP
  | CUR_STD
  | CUR_SVC
  | CUR_SYP
  | CUR_SZL
  | CUR_THB
  | CUR_TJS
  | CUR_TMT
  | CUR_TND
  | CUR_TOP
  | CUR_TRY
  | CUR_TTD
  | CUR_TWD
  | CUR_TZS
  | CUR_UAH
  | CUR_UGX
  | CUR_USD
  | CUR_USN
  | CUR_UYI
  | CUR_UYU
  | CUR_UZS
  | CUR_VEF
  | CUR_VND
  | CUR_VUV
  | CUR_WST
  | CUR_XAF
  | CUR_XAG
  | CUR_XAU
  | CUR_XBA
  | CUR_XBB
  | CUR_XBC
  | CUR_XBD
  | CUR_XCD
  | CUR_XDR
  | CUR_XOF
  | CUR_XPD
  | CUR_XPF
  | CUR_XPT
  | CUR_XSU
  | CUR_XTS
  | CUR_XUA
  | CUR_XXX
  | CUR_YER
  | CUR_ZAR
  | CUR_ZMW
  | CUR_ZWL
  deriving (Eq, Read, Show)

instance Chainable Currency where
  chain = pack . render

instance Type Currency where
  parseExpr = justErr NotInCatalog . readMaybe . ("CUR_" ++)

  render = drop 4 . show

decimalPlaces :: Currency -> Int
decimalPlaces CUR_AED = 2
decimalPlaces CUR_AFN = 2
decimalPlaces CUR_ALL = 2
decimalPlaces CUR_AMD = 2
decimalPlaces CUR_ANG = 2
decimalPlaces CUR_AOA = 2
decimalPlaces CUR_ARS = 2
decimalPlaces CUR_AUD = 2
decimalPlaces CUR_AWG = 2
decimalPlaces CUR_AZN = 2
decimalPlaces CUR_BAM = 2
decimalPlaces CUR_BBD = 2
decimalPlaces CUR_BDT = 2
decimalPlaces CUR_BGN = 2
decimalPlaces CUR_BHD = 3
decimalPlaces CUR_BIF = 0
decimalPlaces CUR_BMD = 2
decimalPlaces CUR_BND = 2
decimalPlaces CUR_BOB = 2
decimalPlaces CUR_BOV = 2
decimalPlaces CUR_BRL = 2
decimalPlaces CUR_BSD = 2
decimalPlaces CUR_BTN = 2
decimalPlaces CUR_BWP = 2
decimalPlaces CUR_BYN = 2
decimalPlaces CUR_BZD = 2
decimalPlaces CUR_CAD = 2
decimalPlaces CUR_CDF = 2
decimalPlaces CUR_CHE = 2
decimalPlaces CUR_CHF = 2
decimalPlaces CUR_CHW = 2
decimalPlaces CUR_CLF = 4
decimalPlaces CUR_CLP = 0
decimalPlaces CUR_CNY = 2
decimalPlaces CUR_COP = 2
decimalPlaces CUR_COU = 2
decimalPlaces CUR_CRC = 2
decimalPlaces CUR_CUC = 2
decimalPlaces CUR_CUP = 2
decimalPlaces CUR_CVE = 0
decimalPlaces CUR_CZK = 2
decimalPlaces CUR_DJF = 0
decimalPlaces CUR_DKK = 2
decimalPlaces CUR_DOP = 2
decimalPlaces CUR_DZD = 2
decimalPlaces CUR_EGP = 2
decimalPlaces CUR_ERN = 2
decimalPlaces CUR_ETB = 2
decimalPlaces CUR_EUR = 2
decimalPlaces CUR_FJD = 2
decimalPlaces CUR_FKP = 2
decimalPlaces CUR_GBP = 2
decimalPlaces CUR_GEL = 2
decimalPlaces CUR_GHS = 2
decimalPlaces CUR_GIP = 2
decimalPlaces CUR_GMD = 2
decimalPlaces CUR_GNF = 0
decimalPlaces CUR_GTQ = 2
decimalPlaces CUR_GYD = 2
decimalPlaces CUR_HKD = 2
decimalPlaces CUR_HNL = 2
decimalPlaces CUR_HRK = 2
decimalPlaces CUR_HTG = 2
decimalPlaces CUR_HUF = 2
decimalPlaces CUR_IDR = 2
decimalPlaces CUR_ILS = 2
decimalPlaces CUR_INR = 2
decimalPlaces CUR_IQD = 3
decimalPlaces CUR_IRR = 2
decimalPlaces CUR_ISK = 0
decimalPlaces CUR_JMD = 2
decimalPlaces CUR_JOD = 3
decimalPlaces CUR_JPY = 0
decimalPlaces CUR_KES = 2
decimalPlaces CUR_KGS = 2
decimalPlaces CUR_KHR = 2
decimalPlaces CUR_KMF = 0
decimalPlaces CUR_KPW = 2
decimalPlaces CUR_KRW = 0
decimalPlaces CUR_KWD = 3
decimalPlaces CUR_KYD = 2
decimalPlaces CUR_KZT = 2
decimalPlaces CUR_LAK = 2
decimalPlaces CUR_LBP = 2
decimalPlaces CUR_LKR = 2
decimalPlaces CUR_LRD = 2
decimalPlaces CUR_LSL = 2
decimalPlaces CUR_LYD = 3
decimalPlaces CUR_MAD = 2
decimalPlaces CUR_MDL = 2
decimalPlaces CUR_MGA = 1
decimalPlaces CUR_MKD = 2
decimalPlaces CUR_MMK = 2
decimalPlaces CUR_MNT = 2
decimalPlaces CUR_MOP = 2
decimalPlaces CUR_MRO = 1
decimalPlaces CUR_MUR = 2
decimalPlaces CUR_MVR = 2
decimalPlaces CUR_MWK = 2
decimalPlaces CUR_MXN = 2
decimalPlaces CUR_MXV = 2
decimalPlaces CUR_MYR = 2
decimalPlaces CUR_MZN = 2
decimalPlaces CUR_NAD = 2
decimalPlaces CUR_NGN = 2
decimalPlaces CUR_NIO = 2
decimalPlaces CUR_NOK = 2
decimalPlaces CUR_NPR = 2
decimalPlaces CUR_NZD = 2
decimalPlaces CUR_OMR = 3
decimalPlaces CUR_PAB = 2
decimalPlaces CUR_PEN = 2
decimalPlaces CUR_PGK = 2
decimalPlaces CUR_PHP = 2
decimalPlaces CUR_PKR = 2
decimalPlaces CUR_PLN = 2
decimalPlaces CUR_PYG = 0
decimalPlaces CUR_QAR = 2
decimalPlaces CUR_RON = 2
decimalPlaces CUR_RSD = 2
decimalPlaces CUR_RUB = 2
decimalPlaces CUR_RWF = 0
decimalPlaces CUR_SAR = 2
decimalPlaces CUR_SBD = 2
decimalPlaces CUR_SCR = 2
decimalPlaces CUR_SDG = 2
decimalPlaces CUR_SEK = 2
decimalPlaces CUR_SGD = 2
decimalPlaces CUR_SHP = 2
decimalPlaces CUR_SLL = 2
decimalPlaces CUR_SOS = 2
decimalPlaces CUR_SRD = 2
decimalPlaces CUR_SSP = 2
decimalPlaces CUR_STD = 2
decimalPlaces CUR_SVC = 2
decimalPlaces CUR_SYP = 2
decimalPlaces CUR_SZL = 2
decimalPlaces CUR_THB = 2
decimalPlaces CUR_TJS = 2
decimalPlaces CUR_TMT = 2
decimalPlaces CUR_TND = 3
decimalPlaces CUR_TOP = 2
decimalPlaces CUR_TRY = 2
decimalPlaces CUR_TTD = 2
decimalPlaces CUR_TWD = 2
decimalPlaces CUR_TZS = 2
decimalPlaces CUR_UAH = 2
decimalPlaces CUR_UGX = 0
decimalPlaces CUR_USD = 2
decimalPlaces CUR_USN = 2
decimalPlaces CUR_UYI = 0
decimalPlaces CUR_UYU = 2
decimalPlaces CUR_UZS = 2
decimalPlaces CUR_VEF = 2
decimalPlaces CUR_VND = 0
decimalPlaces CUR_VUV = 0
decimalPlaces CUR_WST = 2
decimalPlaces CUR_XAF = 0
decimalPlaces CUR_XAG = 0
decimalPlaces CUR_XAU = 0
decimalPlaces CUR_XBA = 0
decimalPlaces CUR_XBB = 0
decimalPlaces CUR_XBC = 0
decimalPlaces CUR_XBD = 0
decimalPlaces CUR_XCD = 2
decimalPlaces CUR_XDR = 0
decimalPlaces CUR_XOF = 0
decimalPlaces CUR_XPD = 0
decimalPlaces CUR_XPF = 0
decimalPlaces CUR_XPT = 0
decimalPlaces CUR_XSU = 0
decimalPlaces CUR_XTS = 0
decimalPlaces CUR_XUA = 0
decimalPlaces CUR_XXX = 0
decimalPlaces CUR_YER = 2
decimalPlaces CUR_ZAR = 2
decimalPlaces CUR_ZMW = 2
decimalPlaces CUR_ZWL = 2
