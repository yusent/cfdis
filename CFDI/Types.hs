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
    rfc  :: String,
    name :: Maybe String
  } deriving (Show)
