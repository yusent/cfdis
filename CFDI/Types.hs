module CFDI.Types where

data CFDI =
  CFDI {
    accountNumber :: Maybe String,
    currency      :: Maybe String,
    subTotal      :: Float,
    signature     :: String,
    total         :: Float,
    _type         :: String
  } deriving (Show)
