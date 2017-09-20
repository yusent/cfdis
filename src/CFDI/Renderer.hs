{-# LANGUAGE OverloadedStrings #-}

module CFDI.Renderer (render) where

import CFDI.Types
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
  )

class Renderable r where
  attributes :: r -> [Attr]
  attributes r =
    []

  children :: r -> [Element]
  children r =
    []

  nodeName :: r -> String

  nodePrefix :: r -> String
  nodePrefix r =
    "cfdi"

  render :: r -> Element
  render r =
    elem (nodePrefix r) (nodeName r) (attributes r) (children r)

instance Renderable Address where
  attributes r =
    [ attr "pais" $ country r
    ] ++ catMaybes
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

  nodeName r =
    "Domicilio"

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

  nodeName r =
    "Comprobante"

instance Renderable Complement where
  children r =
    maybeToList $ render <$> pacStamp r

  nodeName r =
    "Complemento"

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

  nodeName r =
    "Concepto"

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

  nodeName r =
    "Parte"

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

  nodeName r = "DomicilioFiscal"

instance Renderable ImportInfo where
  attributes r =
    [ attr "fecha" . pack . showGregorian $ importIssuedAt r
    , attr "numero" $ importNumber r
    ] ++ catMaybes
    [ attr "aduana" <$> custom r
    ]

  nodeName r = "InformacionAduanera"

instance Renderable Issuer where
  attributes r =
    [ attr "rfc" $ rfc r
    ] ++ catMaybes
    [ attr "nombre" <$> name r
    ]

  children r =
    catMaybes
      [ render <$> fiscalAddress r
      , addrElem
      ]
    ++ (render <$> regimes r)
    where
      addrElem = changeAddrName . render <$> issuedInAddress r
      changeAddrName a = a { elName = QName "ExpedidoEn" Nothing $ Just "cfdi" }

  nodeName r =
    "Emisor"

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

  nodeName r =
    "TimbreFiscalDigital"

  nodePrefix r =
    "tfd"

instance Renderable PropertyAccount where
  attributes r =
    [ attr "numero" $ propertyAccountNumber r
    ]

  nodeName r =
    "CuentaPredial"

instance Renderable Recipient where
  attributes r =
    [ attr "rfc" $ recipientRfc r
    ] ++ catMaybes
    [ attr "nombre" <$> recipientName r
    ]

  children r =
    maybeToList $ render <$> recipientAddress r

  nodeName r =
    "Receptor"

instance Renderable RetainedTax where
  attributes r =
    [ attr "importe" $ retainedTaxAmount r
    , attr "impuesto" . pack . show $ retainedTax r
    ]

  nodeName r =
    "Retencion"

instance Renderable Taxes where
  attributes r =
    catMaybes
      [ attr "totalImpuestosRetenidos"   <$> totalRetained r
      , attr "totalImpuestosTrasladados" <$> totalTransfered r
      ]

  children r =
    [ elem (nodePrefix r) "Retenciones" [] $ render <$> retainedTaxes r
    , elem (nodePrefix r) "Traslados" [] $ render <$> transferedTaxes r
    ]

  nodeName r =
    "Impuestos"

instance Renderable TaxRegime where
  attributes r =
    [ attr "Regimen" $ regime r
    ]

  nodeName r =
    "RegimenFiscal"

instance Renderable TransferedTax where
  attributes r =
    [ attr "importe" $ transferedTaxAmount r
    , attr "impuesto" . pack . show $ transferedTax r
    , attr "tasa" $ transferedTaxRate r
    ]

  nodeName r =
    "Traslado"

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
