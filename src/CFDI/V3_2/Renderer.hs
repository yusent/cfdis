module CFDI.V3_2.Renderer
  ( toXML
  ) where

import CFDI.V3_2.Types
import Data.Maybe          (catMaybes, maybeToList)
import Data.Text           (Text, pack, unpack)
import Data.Time.Calendar  (showGregorian)
import Data.Time.Format    (defaultTimeLocale, formatTime)
import Prelude      hiding (elem)
import Text.XML.Light
  ( Attr(..)
  , Content(Elem)
  , Element(..)
  , QName(..)
  , ppTopElement
  )

class Renderable r where
  attributes :: r -> [Attr]
  attributes =
    const []

  children :: r -> [Element]
  children =
    const []

  nodeName :: r -> String

  nodePrefix :: r -> String
  nodePrefix =
    const "cfdi"

  render :: r -> Element
  render r =
    elem (nodePrefix r) (nodeName r) (attributes r) (children r)

instance Renderable Address where
  attributes r =
    attr "pais" (country r) : catMaybes
      [ attr "noExterior"   <$> externalNumber r
      , attr "noInterior"   <$> internalNumber r
      , attr "localidad"    <$> locality r
      , attr "referencia"   <$> reference r
      , attr "colonia"      <$> suburb r
      , attr "municipio"    <$> municipality r
      , attr "estado"       <$> state r
      , attr "calle"        <$> street r
      , attr "codigoPostal" <$> zipCode r
      ]

  nodeName =
    const "Domicilio"

instance Renderable CFDI where
  attributes r =
    [ attrWithPrefix "xsi" "schemaLocation"
        "http://www.sat.gob.mx/cfd/3 \
        \http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv32.xsd"
    , attrWithPrefix "xmlns" "cfdi" "http://www.sat.gob.mx/cfd/3"
    , attrWithPrefix "xmlns" "xsi" "http://www.w3.org/2001/XMLSchema-instance"
    , attr "certificado"       $ certificate r
    , attr "noCertificado"     $ certificateNumber r
    , attr "LugarExpedicion"   $ issuedIn r
    , attr "metodoDePago"      $ paymentMethod r
    , attr "subTotal"          $ subTotal r
    , attr "sello"             $ signature r
    , attr "total"             $ total r
    , attr "tipoDeComprobante" $ _type r
    , attr "version"           $ version r
    , attr "formaDePago"       $ wayToPay r
    , attr "fecha" . pack
      . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" $ issuedAt r
    ] ++ catMaybes
    [ attr "NumCtaPago"           <$> accountNumber r
    , attr "Moneda"               <$> currency r
    , attr "descuento"            <$> discount r
    , attr "motivoDescuento"      <$> discountReason r
    , attr "TipoCambio"           <$> exchangeRate r
    , attr "folio"                <$> internalID r
    , attr "MontoFolioFiscalOrig" <$> originalAmount r
    , attr "FolioFiscalOrig"      <$> originalNumber r
    , attr "SerieFolioFiscalOrig" <$> originalSeries r
    , attr "condicionesDePago"    <$> paymentConditions r
    , attr "serie"                <$> series r
    , attr "FechaFolioFiscalOrig" . pack
      . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> originalIssuedAt r
    ]

  children r =
    [ render $ issuer r
    , render $ recipient r
    , elem (nodePrefix r) "Conceptos" [] $ render <$> concepts r
    , render $ taxes r
    ] ++ catMaybes
    [ render <$> complement r
    ]

  nodeName =
    const "Comprobante"

instance Renderable Complement where
  children r =
    maybeToList $ render <$> pacStamp r

  nodeName =
    const "Complemento"

instance Renderable Concept where
  attributes r =
    [ attr "importe"       $ amount r
    , attr "descripcion"   $ description r
    , attr "cantidad"      $ quantity r
    , attr "unidad"        $ unit r
    , attr "valorUnitario" $ unitAmount r
    ] ++ catMaybes
    [ attr "noIdentificacion" <$> _id r
    ]

  children r = concat
    [ render <$> importInfo r
    , maybeToList $ render <$> propertyAccount r
    , render <$> parts r
    ]

  nodeName =
    const "Concepto"

instance Renderable ConceptPart where
  attributes r =
    [ attr "descripcion" $ partDescription r
    , attr "cantidad"    $ partQuantity r
    ] ++ catMaybes
    [ attr "importe"          <$> partAmount r
    , attr "noIdentificacion" <$> partId r
    , attr "unidad"           <$> partUnit r
    , attr "valorUnitario"    <$> partUnitAmount r
    ]

  children r =
    render <$> partImportInfo r

  nodeName =
    const "Parte"

instance Renderable FiscalAddress where
  attributes r =
    [ attr "pais"         $ fiscalCountry r
    , attr "municipio"    $ fiscalMunicipality r
    , attr "estado"       $ fiscalState r
    , attr "calle"        $ fiscalStreet r
    , attr "codigoPostal" $ fiscalZipCode r
    ] ++ catMaybes
    [ attr "noExterior" <$> fiscalExternalNumber r
    , attr "noInterior" <$> fiscalInternalNumber r
    , attr "localidad"  <$> fiscalLocality r
    , attr "referencia" <$> fiscalReference r
    , attr "colonia"    <$> fiscalSuburb r
    ]

  nodeName =
    const "DomicilioFiscal"

instance Renderable ImportInfo where
  attributes r =
    [ attr "fecha" . pack . showGregorian $ importIssuedAt r
    , attr "numero" $ importNumber r
    ] ++ catMaybes
    [ attr "aduana" <$> custom r
    ]

  nodeName =
    const "InformacionAduanera"

instance Renderable Issuer where
  attributes r =
    attr "rfc" (rfc r) : catMaybes [attr "nombre" <$> name r]

  children r =
    catMaybes
      [ render <$> fiscalAddress r
      , addrElem
      ]
    ++ (render <$> regimes r)
    where
      addrElem = changeAddrName . render <$> issuedInAddress r
      changeAddrName a = a { elName = QName "ExpedidoEn" Nothing $ Just "cfdi" }

  nodeName =
    const "Emisor"

instance Renderable PacStamp where
  attributes r =
    [ attrWithPrefix "xsi" "schemaLocation"
        "http://www.sat.gob.mx/TimbreFiscalDigital \
        \http://www.sat.gob.mx/TimbreFiscalDigital/TimbreFiscalDigital.xsd"
    , attrWithPrefix "xmlns" "tfd" "http://www.sat.gob.mx/TimbreFiscalDigital"
    , attr "selloCFD"         $ cfdSignature r
    , attr "noCertificadoSAT" $ satCertificateNumber r
    , attr "selloSAT"         $ satSignature r
    , attr "version"          $ stampVersion r
    , attr "UUID"             $ uuid r
    , attr "FechaTimbrado" . pack
      . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" $ stampedAt r
    ]

  nodeName =
    const "TimbreFiscalDigital"

  nodePrefix =
    const "tfd"

instance Renderable PropertyAccount where
  attributes r =
    [ attr "numero" $ propertyAccountNumber r
    ]

  nodeName =
    const "CuentaPredial"

instance Renderable Recipient where
  attributes r =
    attr "rfc" (recipientRfc r) : catMaybes [attr "nombre" <$> recipientName r]

  children r =
    maybeToList $ render <$> recipientAddress r

  nodeName =
    const "Receptor"

instance Renderable RetainedTax where
  attributes r =
    [ attr "importe" $ retainedTaxAmount r
    , attr "impuesto" . pack . show $ retainedTax r
    ]

  nodeName =
    const "Retencion"

instance Renderable Taxes where
  attributes r =
    catMaybes
      [ attr "totalImpuestosRetenidos"   <$> totalRetained r
      , attr "totalImpuestosTrasladados" <$> totalTransfered r
      ]

  children r =
    filter (not . null . elContent)
      [ elem (nodePrefix r) "Retenciones" [] $ render <$> retainedTaxes r
      , elem (nodePrefix r) "Traslados" [] $ render <$> transferedTaxes r
      ]

  nodeName =
    const "Impuestos"

instance Renderable TaxRegime where
  attributes r =
    [ attr "Regimen" $ regime r
    ]

  nodeName =
    const "RegimenFiscal"

instance Renderable TransferedTax where
  attributes r =
    [ attr "importe" $ transferedTaxAmount r
    , attr "impuesto" . pack . show $ transferedTax r
    , attr "tasa" $ transferedTaxRate r
    ]

  nodeName =
    const "Traslado"

attr :: String -> Text -> Attr
attr attrName =
  Attr (QName attrName Nothing Nothing) . unpack

attrWithPrefix :: String -> String -> Text -> Attr
attrWithPrefix prefix attrName =
  Attr (QName attrName Nothing (Just prefix)) . unpack

elem :: String -> String -> [Attr] -> [Element] -> Element
elem prefix elemName attrs elems = Element
  (QName elemName Nothing $ Just prefix)
  attrs
  (Elem <$> elems)
  Nothing

toXML :: CFDI -> Text
toXML = pack . ppTopElement . render
