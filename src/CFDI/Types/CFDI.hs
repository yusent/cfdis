module CFDI.Types.CFDI where

import CFDI.Chainable
import CFDI.Types.Amount
import CFDI.Types.CertificateNumber
import CFDI.Types.CfdiType
import CFDI.Types.Complement
import CFDI.Types.Concepts
import CFDI.Types.Confirmation
import CFDI.Types.Currency
import CFDI.Types.ExchangeRate
import CFDI.Types.Folio
import CFDI.Types.Issuer
import CFDI.Types.PaymentConditions
import CFDI.Types.PaymentMethod
import CFDI.Types.Recipient
import CFDI.Types.RelatedCfdis
import CFDI.Types.Series
import CFDI.Types.Taxes
import CFDI.Types.Version
import CFDI.Types.WayToPay
import CFDI.Types.ZipCode
import CFDI.XmlNode
import Data.Maybe                   (catMaybes)
import Data.Text                    (Text)
import Data.Time.LocalTime          (LocalTime)

data CFDI = CFDI
  { certNum       :: Maybe CertificateNumber
  , certText      :: Maybe Text
  , cfdiType      :: CfdiType
  , complement    :: [Complement]
  , concepts      :: Concepts
  , confirmation  :: Maybe Confirmation
  , currency      :: Currency
  , discount      :: Maybe Amount
  , exchangeRate  :: Maybe ExchangeRate
  , folio         :: Maybe Folio
  , issuedAt      :: LocalTime
  , issuedIn      :: ZipCode
  , issuer        :: Issuer
  , paymentConds  :: Maybe PaymentConditions
  , paymentMethod :: Maybe PaymentMethod
  , recipient     :: Recipient
  , relatedCfdis  :: Maybe RelatedCfdis
  , series        :: Maybe Series
  , signature     :: Maybe Text
  , subTotal      :: Amount
  , taxes         :: Maybe Taxes
  , total         :: Amount
  , version       :: Version
  , wayToPay      :: Maybe WayToPay
  } deriving (Eq, Show)

instance Chainable CFDI where
  chain c = version
        <@> series
        <~> folio
        <~> issuedAt
        <~> wayToPay
        <~> certNum
        <~> paymentConds
        <~> subTotal
        <~> discount
        <~> currency
        <~> exchangeRate
        <~> total
        <~> cfdiType
        <~> paymentMethod
        <~> issuedIn
        <~> confirmation
        <~> relatedCfdis
        <~> issuer
        <~> recipient
        <~> concepts
        <~> taxes
        <~> (c, "")

instance XmlNode CFDI where
  attributes n =
    [ attrWithPrefix "xsi" "schemaLocation"
        ("http://www.sat.gob.mx/cfd/3 \
         \http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv32.xsd" :: Text)
    , attrWithPrefix "xmlns" "cfdi"
        ("http://www.sat.gob.mx/cfd/3" :: Text)
    , attrWithPrefix "xmlns" "xsi"
        ("http://www.w3.org/2001/XMLSchema-instance" :: Text)
    , attr "TipoDeComprobante" $ cfdiType n
    , attr "Moneda"            $ currency n
    , attr "Fecha"             $ issuedAt n
    , attr "LugarExpedicion"   $ issuedIn n
    , attr "SubTotal"          $ subTotal n
    , attr "Total"             $ total n
    , attr "Version"           $ version n
    ] ++ catMaybes
    [ attr "NoCertificado"     <$> certNum n
    , attr "Certificado"       <$> certText n
    , attr "Confirmacion"      <$> confirmation n
    , attr "Descuento"         <$> discount n
    , attr "TipoCambio"        <$> exchangeRate n
    , attr "Folio"             <$> folio n
    , attr "CondicionesDePago" <$> paymentConds n
    , attr "MetodoPago"        <$> paymentMethod n
    , attr "Serie"             <$> series n
    , attr "Sello"             <$> signature n
    , attr "FormaPago"         <$> wayToPay n
    ]

  children r = catMaybes
    [ renderNode <$> relatedCfdis r
    , Just . renderNode $ issuer r
    , Just . renderNode $ recipient r
    , Just . renderNode $ concepts r
    , renderNode <$> taxes r
    ] ++ map renderNode (complement r)

  nodeName = const "Comprobante"

  parseNode n = CFDI
    <$> parseAttribute "NoCertificado" n
    <*> parseAttribute "Certificado" n
    <*> requireAttribute "TipoDeComprobante" n
    <*> parseChildren "Complemento" n
    <*> requireChild "Conceptos" n
    <*> parseAttribute "Confirmacion" n
    <*> requireAttribute "Moneda" n
    <*> parseAttribute "Descuento" n
    <*> parseAttribute "TipoCambio" n
    <*> parseAttribute "Folio" n
    <*> requireAttribute "Fecha" n
    <*> requireAttribute "LugarExpedicion" n
    <*> requireChild "Emisor" n
    <*> parseAttribute "CondicionesDePago" n
    <*> parseAttribute "MetodoPago" n
    <*> requireChild "Receptor" n
    <*> parseChild "CfdiRelacionados" n
    <*> parseAttribute "Serie" n
    <*> parseAttribute "Sello" n
    <*> requireAttribute "SubTotal" n
    <*> parseChild "Impuestos" n
    <*> requireAttribute "Total" n
    <*> requireAttribute "Version" n
    <*> parseAttribute "FormaPago" n
