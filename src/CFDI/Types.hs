module CFDI.Types where

import Data.Time.Calendar  (Day)
import Data.Time.LocalTime (LocalTime)

data Address = Address
  { country        :: String
  , externalNumber :: Maybe String
  , internalNumber :: Maybe String
  , locality       :: Maybe String
  , municipality   :: Maybe String
  , reference      :: Maybe String
  , suburb         :: Maybe String
  , state          :: Maybe String
  , street         :: Maybe String
  , zipCode        :: Maybe String
  } deriving (Show)

data CFDI = CFDI
  { accountNumber     :: Maybe String
  , certificate       :: String
  , certificateNumber :: String
  , concepts          :: [Concept]
  , currency          :: Maybe String
  , internalID        :: Maybe String
  , issuedAt          :: LocalTime
  , issuedIn          :: String
  , issuer            :: Issuer
  , paymentConditions :: Maybe String
  , paymentMethod     :: String
  , recipient         :: Recipient
  , subTotal          :: Float
  , signature         :: String
  , total             :: Float
  , _type             :: String
  , version           :: String
  } deriving (Show)

data Concept = Concept
  { amount          :: Float
  , description     :: String
  , _id             :: Maybe String
  , importInfo      :: [ImportInfo]
  , parts           :: [ConceptPart]
  , propertyAccount :: Maybe PropertyAccount
  , quantity        :: Float
  , unit            :: String
  , unitAmount      :: Float
  } deriving (Show)

data ConceptPart = ConceptPart
  { partAmount      :: Maybe Float
  , partDescription :: String
  , partId          :: Maybe String
  , partImportInfo  :: [ImportInfo]
  , partQuantity    :: Float
  , partUnit        :: Maybe String
  , partUnitAmount  :: Maybe Float
  } deriving (Show)

data ImportInfo = ImportInfo
  { custom         :: Maybe String
  , importIssuedAt :: Day
  , importNumber   :: String
  } deriving (Show)

data FiscalAddress = FiscalAddress
  { fiscalCountry        :: String
  , fiscalExternalNumber :: Maybe String
  , fiscalInternalNumber :: Maybe String
  , fiscalLocality       :: Maybe String
  , fiscalMunicipality   :: String
  , fiscalReference      :: Maybe String
  , fiscalState          :: String
  , fiscalStreet         :: String
  , fiscalSuburb         :: Maybe String
  , fiscalZipCode        :: String
  } deriving (Show)

data Issuer = Issuer
  { fiscalAddress   :: Maybe FiscalAddress
  , issuedInAddress :: Maybe Address
  , name            :: Maybe String
  , regimes         :: [TaxRegime]
  , rfc             :: String
  } deriving (Show)

data PropertyAccount = PropertyAccount
  { propertyAccountNumber :: String
  } deriving (Show)

data Recipient = Recipient
  { recipientAddress :: Maybe Address
  , recipientName    :: Maybe String
  , recipientRfc     :: String
  } deriving (Show)

data TaxRegime = TaxRegime
  { regime :: String
  } deriving (Show)
