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
  nodeName = const "TimbreFiscalDigital"

  nodePrefix = const "tfd"

  optionalAttributes n = [attr "Leyenda" <$> psLegend n]

  parseNode n = PacStamp
    <$> parseAttribute "Leyenda" n
    <*> requireAttribute "RfcProvCertif" n
    <*> requireAttribute "NoCertificadoSAT" n
    <*> requireAttribute "SelloSAT" n
    <*> requireAttribute "SelloCFD" n
    <*> requireAttribute "FechaTimbrado" n
    <*> requireAttribute "UUID" n
    <*> requireAttribute "Version" n

  requiredAttributes n =
    [ attrWithPrefix "xsi" "schemaLocation"
        ("http://www.sat.gob.mx/TimbreFiscalDigital http://\
         \www.sat.gob.mx/TimbreFiscalDigital/TimbreFiscalDigital.xsd" :: Text)
    , attrWithPrefix "xmlns" "tfd"
        ("http://www.sat.gob.mx/TimbreFiscalDigital" :: Text)
    , attr "RfcProvCertif"    $ psPacRfc n
    , attr "NoCertificadoSAT" $ psSatCerNum n
    , attr "SelloSAT"         $ psSatSig n
    , attr "SelloCFD"         $ psSignature n
    , attr "FechaTimbrado"    $ psStampedAt n
    , attr "UUID"             $ psUuid n
    , attr "Version"          $ psVersion n
    ]
