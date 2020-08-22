module CFDI.V3_2.Types where

import Data.Text           (Text)
import Data.Time.Calendar  (Day)
import Data.Time.LocalTime (LocalTime)

data Address = Address
  { country        :: Text
  , externalNumber :: Maybe Text
  , internalNumber :: Maybe Text
  , locality       :: Maybe Text
  , municipality   :: Maybe Text
  , reference      :: Maybe Text
  , suburb         :: Maybe Text
  , state          :: Maybe Text
  , street         :: Maybe Text
  , zipCode        :: Maybe Text
  } deriving (Eq, Show)

data CFDI = CFDI
  { accountNumber     :: Maybe Text
  , certificate       :: Text
  , certificateNumber :: Text
  , complement        :: Maybe Complement
  , concepts          :: [Concept]
  , currency          :: Maybe Text
  , discount          :: Maybe Text
  , discountReason    :: Maybe Text
  , exchangeRate      :: Maybe Text
  , internalID        :: Maybe Text
  , issuedAt          :: LocalTime
  , issuedIn          :: Text
  , issuer            :: Issuer
  , originalAmount    :: Maybe Text
  , originalIssuedAt  :: Maybe LocalTime
  , originalNumber    :: Maybe Text
  , originalSeries    :: Maybe Text
  , paymentConditions :: Maybe Text
  , paymentMethod     :: Text
  , recipient         :: Recipient
  , series            :: Maybe Text
  , subTotal          :: Text
  , signature         :: Text
  , taxes             :: Taxes
  , total             :: Text
  , _type             :: Text
  , version           :: Text
  , wayToPay          :: Text
  } deriving (Eq, Show)

newtype Complement = Complement
  { pacStamp :: Maybe PacStamp
  } deriving (Eq, Show)

data Concept = Concept
  { amount          :: Text
  , description     :: Text
  , _id             :: Maybe Text
  , importInfo      :: [ImportInfo]
  , parts           :: [ConceptPart]
  , propertyAccount :: Maybe PropertyAccount
  , quantity        :: Text
  , unit            :: Text
  , unitAmount      :: Text
  } deriving (Eq, Show)

data ConceptPart = ConceptPart
  { partAmount      :: Maybe Text
  , partDescription :: Text
  , partId          :: Maybe Text
  , partImportInfo  :: [ImportInfo]
  , partQuantity    :: Text
  , partUnit        :: Maybe Text
  , partUnitAmount  :: Maybe Text
  } deriving (Eq, Show)

data FiscalAddress = FiscalAddress
  { fiscalCountry        :: Text
  , fiscalExternalNumber :: Maybe Text
  , fiscalInternalNumber :: Maybe Text
  , fiscalLocality       :: Maybe Text
  , fiscalMunicipality   :: Text
  , fiscalReference      :: Maybe Text
  , fiscalState          :: Text
  , fiscalStreet         :: Text
  , fiscalSuburb         :: Maybe Text
  , fiscalZipCode        :: Text
  } deriving (Eq, Show)

data ImportInfo = ImportInfo
  { custom         :: Maybe Text
  , importIssuedAt :: Day
  , importNumber   :: Text
  } deriving (Eq, Show)

data Issuer = Issuer
  { fiscalAddress   :: Maybe FiscalAddress
  , issuedInAddress :: Maybe Address
  , name            :: Maybe Text
  , regimes         :: [TaxRegime]
  , rfc             :: Text
  } deriving (Eq, Show)

data PacStamp = PacStamp
  { cfdSignature         :: Text
  , satCertificateNumber :: Text
  , satSignature         :: Text
  , stampedAt            :: LocalTime
  , stampVersion         :: Text
  , uuid                 :: Text
  } deriving (Eq, Show)

newtype PropertyAccount = PropertyAccount
  { propertyAccountNumber :: Text
  } deriving (Eq, Show)

data Recipient = Recipient
  { recipientAddress :: Maybe Address
  , recipientName    :: Maybe Text
  , recipientRfc     :: Text
  } deriving (Eq, Show)

data RetainedTax = RetainedTax
  { retainedTaxAmount :: Text
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
  , totalRetained   :: Maybe Text
  , totalTransfered :: Maybe Text
  } deriving (Eq, Show)

newtype TaxRegime = TaxRegime
  { regime :: Text
  } deriving (Eq, Show)

data TransferedTax = TransferedTax
  { transferedTaxAmount :: Text
  , transferedTaxRate   :: Text
  , transferedTax       :: Tax
  } deriving (Eq, Show)
