module CFDI.Parser (parseCFDI) where

import Data.Maybe           (fromJust, fromMaybe)
import Data.Time.Calendar   (Day)
import Data.Time.LocalTime  (LocalTime, localDay)
import Data.Time.Parse      (strptime)
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Lexer (XmlSource)
import Text.XML.Light.Proc  (filterElementName, filterElementsName, findAttrBy)
import Text.XML.Light.Types (Element(Element), QName(QName))
import CFDI.Types

findAttrValueByName :: String -> Element -> Maybe String
findAttrValueByName attrName =
  findAttrBy (nameEquals attrName)

findChildByName :: String -> Element -> Maybe Element
findChildByName childName =
  filterElementName (nameEquals childName)

findChildrenByName :: String -> Element -> [Element]
findChildrenByName childName =
  filterElementsName (nameEquals childName)

nameEquals :: String -> QName -> Bool
nameEquals s (QName name _ _) =
  s == name

parseAddress :: Element -> Address
parseAddress element = Address
  { country        = requireAttrValueByName "pais" element
  , externalNumber = findAttrValueByName "noExterior" element
  , internalNumber = findAttrValueByName "noInterior" element
  , locality       = findAttrValueByName "localidad" element
  , municipality   = findAttrValueByName "municipio" element
  , reference      = findAttrValueByName "referencia" element
  , suburb         = findAttrValueByName "colonia" element
  , state          = findAttrValueByName "estado" element
  , street         = findAttrValueByName "calle" element
  , zipCode        = findAttrValueByName "codigoPostal" element
  }

parseCFDI :: XmlSource s => s -> Maybe CFDI
parseCFDI xmlSource =
  parseCFDIv3_2 <$> parseXMLDoc xmlSource

parseCFDIv3_2 :: Element -> CFDI
parseCFDIv3_2 root = CFDI
  { accountNumber     = findAttrValueByName "NumCtaPago" root
  , certificate       = requireAttrValueByName "certificado" root
  , certificateNumber = requireAttrValueByName "noCertificado" root
  , complement        = fmap parseComplement
                      $ findChildByName "Complemento" root
  , concepts          = map parseConcept
                      . findChildrenByName "Concepto"
                      $ requireChildByName "Conceptos" root
  , currency          = findAttrValueByName "Moneda" root
  , internalID        = findAttrValueByName "folio" root
  , issuedAt          = parseDateTime $ requireAttrValueByName "fecha" root
  , issuedIn          = requireAttrValueByName "LugarExpedicion" root
  , issuer            = parseIssuer $ requireChildByName "Emisor" root
  , paymentConditions = findAttrValueByName "condicionesDePago" root
  , paymentMethod     = requireAttrValueByName "metodoDePago" root
  , recipient         = parseRecipient $ requireChildByName "Receptor" root
  , subTotal          = read $ requireAttrValueByName "subTotal" root
  , signature         = fromMaybe "" $ findAttrValueByName "sello" root
  , taxes             = parseTaxes $ requireChildByName "Impuestos" root
  , total             = read $ requireAttrValueByName "total" root
  , _type             = requireAttrValueByName "tipoDeComprobante" root
  , version           = requireAttrValueByName "version" root
  }

parseComplement :: Element -> Complement
parseComplement element = Complement
  { pacStamp = parsePacStamp <$> findChildByName "TimbreFiscalDigital" element
  }

parseConcept :: Element -> Concept
parseConcept element = Concept
  { amount          = read $ requireAttrValueByName "importe" element
  , description     = requireAttrValueByName "descripcion" element
  , _id             = findAttrValueByName "noIdentificacion" element
  , importInfo      = map parseImportInfo
                    $ findChildrenByName "InformacionAduanera" element
  , parts           = parseConceptPart <$> findChildrenByName "Parte" element
  , propertyAccount = fmap parsePropertyAccount
                    $ findChildByName "CuentaPredial" element
  , quantity        = read $ requireAttrValueByName "cantidad" element
  , unit            = requireAttrValueByName "unidad" element
  , unitAmount      = read $ requireAttrValueByName "valorUnitario" element
  }

parseConceptPart :: Element -> ConceptPart
parseConceptPart element = ConceptPart
  { partAmount      = read <$> findAttrValueByName "importe" element
  , partDescription = requireAttrValueByName "descripcion" element
  , partId          = findAttrValueByName "noIdentificacion" element
  , partImportInfo  = map parseImportInfo
                    $ findChildrenByName "InformacionAduanera" element
  , partQuantity    = read $ requireAttrValueByName "cantidad" element
  , partUnit        = findAttrValueByName "unidad" element
  , partUnitAmount  = read <$> findAttrValueByName "valorUnitario" element
  }

parseDate :: String -> Day
parseDate =
  localDay . fst . fromJust . strptime "%Y-%m-%d"

parseDateTime :: String -> LocalTime
parseDateTime =
  fst . fromJust . strptime "%Y-%m-%dT%H:%M:%S"

parseFiscalAddress :: Element -> FiscalAddress
parseFiscalAddress element = FiscalAddress
  { fiscalCountry        = requireAttrValueByName "pais" element
  , fiscalExternalNumber = findAttrValueByName "noExterior" element
  , fiscalInternalNumber = findAttrValueByName "noInterior" element
  , fiscalLocality       = findAttrValueByName "localidad" element
  , fiscalMunicipality   = requireAttrValueByName "municipio" element
  , fiscalReference      = findAttrValueByName "referencia" element
  , fiscalSuburb         = findAttrValueByName "colonia" element
  , fiscalState          = requireAttrValueByName "estado" element
  , fiscalStreet         = requireAttrValueByName "calle" element
  , fiscalZipCode        = requireAttrValueByName "codigoPostal" element
  }

parseImportInfo :: Element -> ImportInfo
parseImportInfo element = ImportInfo
  { custom         = findAttrValueByName "aduana" element
  , importIssuedAt = parseDate $ requireAttrValueByName "fecha" element
  , importNumber   = requireAttrValueByName "numero" element
  }

parseIssuer :: Element -> Issuer
parseIssuer element = Issuer
  { fiscalAddress   = fmap parseFiscalAddress
                    $ findChildByName "DomicilioFiscal" element
  , issuedInAddress = fmap parseAddress
                    $ findChildByName "ExpedidoEn" element
  , name            = findAttrValueByName "nombre" element
  , rfc             = requireAttrValueByName "rfc" element
  , regimes         = map parseTaxRegime
                    $ findChildrenByName "RegimenFiscal" element
  }

parsePacStamp :: Element -> PacStamp
parsePacStamp element = PacStamp
  { cfdSignature         = requireAttrValueByName "selloCFD" element
  , satCertificateNumber = requireAttrValueByName "noCertificadoSAT" element
  , satSignature         = requireAttrValueByName "selloSAT" element
  , stampVersion         = requireAttrValueByName "version" element
  , stampedAt            = parseDateTime
                         $ requireAttrValueByName "FechaTimbrado" element
  , uuid                 = requireAttrValueByName "UUID" element
  }

parsePropertyAccount :: Element -> PropertyAccount
parsePropertyAccount element = PropertyAccount
  { propertyAccountNumber = requireAttrValueByName "numero" element
  }

parseRecipient :: Element -> Recipient
parseRecipient element = Recipient
  { recipientAddress = parseAddress <$> findChildByName "Domicilio" element
  , recipientName    = findAttrValueByName "nombre" element
  , recipientRfc     = requireAttrValueByName "rfc" element
  }

parseRetainedTax :: Element -> RetainedTax
parseRetainedTax element = RetainedTax
  { retainedTaxAmount = read $ requireAttrValueByName "importe" element
  , retainedTax       = read $ requireAttrValueByName "impuesto" element
  }

parseTaxes :: Element -> Taxes
parseTaxes element = Taxes
  { retainedTaxes   = maybe [] (map parseRetainedTax)
                    . fmap (findChildrenByName "Retencion")
                    $ findChildByName "Retenciones" element
  , transferedTaxes = maybe [] (map parseTransferedTax)
                    . fmap (findChildrenByName "Traslado")
                    $ findChildByName "Traslados" element
  , totalRetained   = fmap read
                    $ findAttrValueByName "totalImpuestosRetenidos" element
  , totalTransfered = fmap read
                    $ findAttrValueByName "totalImpuestosTrasladados" element
  }

parseTaxRegime :: Element -> TaxRegime
parseTaxRegime element = TaxRegime
  { regime = requireAttrValueByName "Regimen" element
  }

parseTransferedTax :: Element -> TransferedTax
parseTransferedTax element = TransferedTax
  { transferedTaxAmount = read $ requireAttrValueByName "importe" element
  , transferedTaxRate   = read $ requireAttrValueByName "tasa" element
  , transferedTax       = read $ requireAttrValueByName "impuesto" element
  }

requireAttrValueByName :: String -> Element -> String
requireAttrValueByName attrName =
  fromJust . findAttrValueByName attrName

requireChildByName :: String -> Element -> Element
requireChildByName childName =
  fromJust . findChildByName childName
