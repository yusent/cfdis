module CFDI.Types where

import Data.Time.LocalTime (LocalTime)

data CFDI =
  CFDI {
    accountNumber :: Maybe String,
    currency      :: Maybe String,
    subTotal      :: Float,
    signature     :: String,
    total         :: Float,
    _type         :: String
  } deriving (Show)
