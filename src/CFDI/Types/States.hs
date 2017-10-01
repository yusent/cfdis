module CFDI.Types.States where

import CFDI.Types.Catalog
import Data.Text    (pack, unpack)
import Text.Read    (readMaybe)

data State
  = ST_AGU
  | ST_BCN
  | ST_BCS
  | ST_CAM
  | ST_CHP
  | ST_CHH
  | ST_COA
  | ST_COL
  | ST_DIF
  | ST_DUR
  | ST_GUA
  | ST_GRO
  | ST_HID
  | ST_JAL
  | ST_MEX
  | ST_MIC
  | ST_MOR
  | ST_NAY
  | ST_NLE
  | ST_OAX
  | ST_PUE
  | ST_QUE
  | ST_ROO
  | ST_SLP
  | ST_SIN
  | ST_SON
  | ST_TAB
  | ST_TAM
  | ST_TLA
  | ST_VER
  | ST_YUC
  | ST_ZAC
  | ST_AL
  | ST_AK
  | ST_AZ
  | ST_AR
  | ST_CA
  | ST_NC
  | ST_SC
  | ST_CO
  | ST_CT
  | ST_ND
  | ST_SD
  | ST_DE
  | ST_FL
  | ST_GA
  | ST_HI
  | ST_ID
  | ST_IL
  | ST_IN
  | ST_IA
  | ST_KS
  | ST_KY
  | ST_LA
  | ST_ME
  | ST_MD
  | ST_MA
  | ST_MI
  | ST_MN
  | ST_MS
  | ST_MO
  | ST_MT
  | ST_NE
  | ST_NV
  | ST_NJ
  | ST_NY
  | ST_NH
  | ST_NM
  | ST_OH
  | ST_OK
  | ST_OR
  | ST_PA
  | ST_RI
  | ST_TN
  | ST_TX
  | ST_UT
  | ST_VT
  | ST_VA
  | ST_WV
  | ST_WA
  | ST_WI
  | ST_WY
  | ST_ON
  | ST_QC
  | ST_NS
  | ST_NB
  | ST_MB
  | ST_BC
  | ST_PE
  | ST_SK
  | ST_AB
  | ST_NL
  | ST_NT
  | ST_YT
  | ST_UN
  deriving (Eq, Read, Show)

instance Catalog State where
  fromCode = readMaybe . ("ST_" ++) . unpack

  toCode = pack . drop 3 . show
