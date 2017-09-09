module CFDI.Types where

import Data.Time.LocalTime (LocalTime)

data CFDI =
  CFDI {
    accountNumber     :: Maybe String,
    certificate       :: String,
    certificateNumber :: String,
    currency          :: Maybe String,
    expeditionPlace   :: String,
    internalID        :: Maybe String,
    issuer            :: Issuer,
    issuedAt          :: LocalTime,
    paymentConditions :: Maybe String,
    paymentMethod     :: String,
    subTotal          :: Float,
    signature         :: String,
    total             :: Float,
    _type             :: String,
    version           :: String
  } deriving (Show)

data Issuer =
  Issuer {
    fiscalAddress :: Maybe FiscalAddress,
    rfc           :: String,
    name          :: Maybe String
  } deriving (Show)

data FiscalAddress =
  FiscalAddress {
    country        :: String,
    externalNumber :: Maybe String,
    internalNumber :: Maybe String,
    locality       :: Maybe String,
    municipality   :: String,
    reference      :: Maybe String,
    suburb         :: Maybe String,
    state          :: String,
    street         :: String,
    zipCode        :: String
  } deriving (Show)
