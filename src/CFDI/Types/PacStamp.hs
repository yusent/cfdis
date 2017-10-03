module CFDI.Types.PacStamp where

import CFDI.Types.CertificateNumber
import CFDI.Types.CompanyRfc
import CFDI.Types.PacStampVersion
import CFDI.Types.SatLegend
import CFDI.Types.UUID
import CFDI.XmlNode
import Data.Text                  (Text)
import Data.Time.LocalTime        (LocalTime)

data PacStamp = PacStamp
  { psLegend    :: Maybe SatLegend
  , psPacRfc    :: CompanyRfc
  , psSatCerNum :: CertificateNumber
  , psSatSig    :: Text
  , psSignature :: Text
  , psStampedAt :: LocalTime
  , psUuid      :: UUID
  , psVersion   :: PacStampVersion
  } deriving (Eq, Show)

instance XmlNode PacStamp where
  parseNode n = PacStamp
    <$> parseAttribute "Leyenda" n
    <*> requireAttribute "RfcProvCertif" n
    <*> requireAttribute "NoCertificadoSAT" n
    <*> requireAttribute "SelloSAT" n
    <*> requireAttribute "SelloCFD" n
    <*> requireAttribute "FechaTimbrado" n
    <*> requireAttribute "UUID" n
    <*> requireAttribute "Version" n
