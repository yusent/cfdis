module CFDI where

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
  } deriving (Eq, Show)

data CFDI = CFDI
  { accountNumber     :: Maybe String
  , certificate       :: String
  , certificateNumber :: String
  , complement        :: Maybe Complement
  , concepts          :: [Concept]
  , currency          :: Maybe String
  , discount          :: Maybe String
  , discountReason    :: Maybe String
  , exchangeRate      :: Maybe String
  , internalID        :: Maybe String
  , issuedAt          :: LocalTime
  , issuedIn          :: String
  , issuer            :: Issuer
  , originalAmount    :: Maybe String
  , originalIssuedAt  :: Maybe LocalTime
  , originalNumber    :: Maybe String
  , originalSeries    :: Maybe String
  , paymentConditions :: Maybe String
  , paymentMethod     :: String
  , recipient         :: Recipient
  , series            :: Maybe String
  , subTotal          :: String
  , signature         :: String
  , taxes             :: Taxes
  , total             :: String
  , _type             :: String
  , version           :: String
  , wayToPay          :: String
  } deriving (Eq, Show)

data Complement = Complement
  { pacStamp :: Maybe PacStamp
  } deriving (Eq, Show)

data Concept = Concept
  { amount          :: String
  , description     :: String
  , _id             :: Maybe String
  , importInfo      :: [ImportInfo]
  , parts           :: [ConceptPart]
  , propertyAccount :: Maybe PropertyAccount
  , quantity        :: String
  , unit            :: String
  , unitAmount      :: String
  } deriving (Eq, Show)

data ConceptPart = ConceptPart
  { partAmount      :: Maybe String
  , partDescription :: String
  , partId          :: Maybe String
  , partImportInfo  :: [ImportInfo]
  , partQuantity    :: String
  , partUnit        :: Maybe String
  , partUnitAmount  :: Maybe String
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

data ImportInfo = ImportInfo
  { custom         :: Maybe String
  , importIssuedAt :: Day
  , importNumber   :: String
  } deriving (Eq, Show)

data Issuer = Issuer
  { fiscalAddress   :: Maybe FiscalAddress
  , issuedInAddress :: Maybe Address
  , name            :: Maybe String
  , regimes         :: [TaxRegime]
  , rfc             :: String
  } deriving (Eq, Show)

data PacStamp = PacStamp
  { cfdSignature         :: String
  , satCertificateNumber :: String
  , satSignature         :: String
  , stampedAt            :: LocalTime
  , stampVersion         :: String
  , uuid                 :: String
  } deriving (Eq, Show)

data PropertyAccount = PropertyAccount
  { propertyAccountNumber :: String
  } deriving (Eq, Show)

data Recipient = Recipient
  { recipientAddress :: Maybe Address
  , recipientName    :: Maybe String
  , recipientRfc     :: String
  } deriving (Eq, Show)

data RetainedTax = RetainedTax
  { retainedTaxAmount :: String
  , retainedTax       :: Tax
  } deriving (Eq, Show)

data Tax
  = IEPS
  | ISR
  | IVA
  deriving (Eq, Read, Show)

data Taxes = Taxes
  { retainedTaxes   :: [RetainedTax]
  , transferedTaxes :: [TransferedTax]
  , totalRetained   :: Maybe String
  , totalTransfered :: Maybe String
  } deriving (Eq, Show)

data TaxRegime = TaxRegime
  { regime :: String
  } deriving (Eq, Show)

data TransferedTax = TransferedTax
  { transferedTaxAmount :: String
  , transferedTaxRate   :: String
  , transferedTax       :: Tax
  } deriving (Eq, Show)
