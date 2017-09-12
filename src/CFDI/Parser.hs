module CFDI.Parser (parseCFDI) where

import CFDI
import Control.Error.Safe   (justErr)
import Control.Monad        (forM, sequence)
import Data.Maybe           (fromJust, fromMaybe)
import Data.Time.Calendar   (Day)
import Data.Time.LocalTime  (LocalTime, localDay)
import Data.Time.Parse      (strptime)
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Lexer (XmlSource)
import Text.XML.Light.Proc  (filterElementName, filterElementsName, findAttrBy)
import Text.XML.Light.Types (Element(Element), QName(QName))

type Error = String

-- Main functions

parseCFDI :: XmlSource s => s -> Either Error CFDI
parseCFDI xmlSource =
  justErr "Could not parse XML" (parseXMLDoc xmlSource) >>= parseCFDIv3_2

parseCFDIv3_2 :: Element -> Either Error CFDI
parseCFDIv3_2 root = CFDI
  <$> parseAttribute "NumCtaPago" root
  <*> requireAttrValueByName "certificado" root
  <*> requireAttrValueByName "noCertificado" root
  <*> parseElementWith parseComplement "Complemento" root
  <*> do
    conceptsNode <- requireChildByName "Conceptos" root
    parseChildrenWith parseConcept "Concepto" conceptsNode
  <*> parseAttribute "Moneda" root
  <*> parseAndReadAttribute "descuento" root
  <*> parseAttribute "motivoDescuento" root
  <*> parseAttribute "folio" root
  <*> requireAndParseAttrWith parseDateTime "fecha" root
  <*> requireAttrValueByName "LugarExpedicion" root
  <*> requireAndParseChildWith parseIssuer "Emisor" root
  <*> parseAndReadAttribute "MontoFolioFiscalOrig" root
  <*> parseAttributeWith parseDateTime "FechaFolioFiscalOrig" root
  <*> parseAttribute "FolioFiscalOrig" root
  <*> parseAttribute "SerieFolioFiscalOrig" root
  <*> parseAttribute "condicionesDePago" root
  <*> requireAttrValueByName "metodoDePago" root
  <*> requireAndParseChildWith parseRecipient "Receptor" root
  <*> parseAttribute "serie" root
  <*> requireAndReadAttribute "subTotal" root
  <*> Right (fromMaybe "" $ findAttrValueByName "sello" root)
  <*> requireAndParseChildWith parseTaxes "Impuestos" root
  <*> requireAndReadAttribute "total" root
  <*> requireAttrValueByName "tipoDeComprobante" root
  <*> requireAttrValueByName "version" root
  <*> requireAttrValueByName "formaDePago" root

-- Parsers

parseAddress :: Element -> Either Error Address
parseAddress element = Address
  <$> requireAttrValueByName "pais" element
  <*> parseAttribute "noExterior" element
  <*> parseAttribute "noInterior" element
  <*> parseAttribute "localidad" element
  <*> parseAttribute "municipio" element
  <*> parseAttribute "referencia" element
  <*> parseAttribute "colonia" element
  <*> parseAttribute "estado" element
  <*> parseAttribute "calle" element
  <*> parseAttribute "codigoPostal" element

parseComplement :: Element -> Either Error Complement
parseComplement element = Complement
  <$> parseElementWith parsePacStamp "TimbreFiscalDigital" element

parseConcept :: Element -> Either Error Concept
parseConcept element = Concept
  <$> requireAndReadAttribute "importe" element
  <*> requireAttrValueByName "descripcion" element
  <*> parseAttribute "noIdentificacion" element
  <*> parseChildrenWith parseImportInfo "InformacionAduanera" element
  <*> parseChildrenWith parseConceptPart "Parte" element
  <*> parseElementWith parsePropertyAccount "CuentaPredial" element
  <*> requireAndReadAttribute "cantidad" element
  <*> requireAttrValueByName "unidad" element
  <*> requireAndReadAttribute "valorUnitario" element

parseConceptPart :: Element -> Either Error ConceptPart
parseConceptPart element = ConceptPart
  <$> parseAndReadAttribute "importe" element
  <*> requireAttrValueByName "descripcion" element
  <*> parseAttribute "noIdentificacion" element
  <*> parseChildrenWith parseImportInfo "InformacionAduanera" element
  <*> requireAndReadAttribute "cantidad" element
  <*> parseAttribute "unidad" element
  <*> parseAndReadAttribute "valorUnitario" element

parseFiscalAddress :: Element -> Either Error FiscalAddress
parseFiscalAddress element = FiscalAddress
  <$> requireAttrValueByName "pais" element
  <*> parseAttribute "noExterior" element
  <*> parseAttribute "noInterior" element
  <*> parseAttribute "localidad" element
  <*> requireAttrValueByName "municipio" element
  <*> parseAttribute "referencia" element
  <*> requireAttrValueByName "estado" element
  <*> requireAttrValueByName "calle" element
  <*> parseAttribute "colonia" element
  <*> requireAttrValueByName "codigoPostal" element

parseImportInfo :: Element -> Either Error ImportInfo
parseImportInfo element = ImportInfo
  <$> parseAttribute "aduana" element
  <*> requireAndParseAttrWith parseDate "fecha" element
  <*> requireAttrValueByName "numero" element

parseIssuer :: Element -> Either Error Issuer
parseIssuer element = Issuer
  <$> parseElementWith parseFiscalAddress "DomicilioFiscal" element
  <*> parseElementWith parseAddress "ExpedidoEn" element
  <*> parseAttribute "nombre" element
  <*> parseChildrenWith parseTaxRegime "RegimenFiscal" element
  <*> requireAttrValueByName "rfc" element

parsePacStamp :: Element -> Either Error PacStamp
parsePacStamp element = PacStamp
  <$> requireAttrValueByName "selloCFD" element
  <*> requireAttrValueByName "noCertificadoSAT" element
  <*> requireAttrValueByName "selloSAT" element
  <*> requireAndParseAttrWith parseDateTime "FechaTimbrado" element
  <*> requireAttrValueByName "version" element
  <*> requireAttrValueByName "UUID" element

parsePropertyAccount :: Element -> Either Error PropertyAccount
parsePropertyAccount element = PropertyAccount
  <$> requireAttrValueByName "numero" element

parseRecipient :: Element -> Either Error Recipient
parseRecipient element = Recipient
  <$> parseElementWith parseAddress "Domicilio" element
  <*> parseAttribute "nombre" element
  <*> requireAttrValueByName "rfc" element

parseRetainedTax :: Element -> Either Error RetainedTax
parseRetainedTax element = RetainedTax
  <$> requireAndReadAttribute "importe" element
  <*> requireAndReadAttribute "impuesto" element

parseTaxes :: Element -> Either Error Taxes
parseTaxes element = Taxes
  <$> sequence rt
  <*> sequence tt
  <*> parseAndReadAttribute "totalImpuestosRetenidos" element
  <*> parseAndReadAttribute "totalImpuestosTrasladados" element
  where
    rt = maybe [] (map parseRetainedTax)
       . fmap (findChildrenByName "Retencion")
       $ findChildByName "Retenciones" element
    tt = maybe [] (map parseTransferedTax)
       . fmap (findChildrenByName "Traslado")
       $ findChildByName "Traslados" element

parseTaxRegime :: Element -> Either Error TaxRegime
parseTaxRegime element = TaxRegime
  <$> requireAttrValueByName "Regimen" element

parseTransferedTax :: Element -> Either Error TransferedTax
parseTransferedTax element = TransferedTax
  <$> requireAndReadAttribute "importe" element
  <*> requireAndReadAttribute "tasa" element
  <*> requireAndReadAttribute "impuesto" element

-- Helpers

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

parseAndReadAttribute :: Read r => String -> Element -> Either Error (Maybe r)
parseAndReadAttribute attrName =
  Right . fmap read . findAttrValueByName attrName

parseAttribute :: String -> Element -> Either Error (Maybe String)
parseAttribute attrName =
  Right . findAttrValueByName attrName

parseAttributeWith
  :: (String -> Either Error a) -> String -> Element -> Either Error (Maybe a)
parseAttributeWith func attrName =
  maybe (Right Nothing) (fmap Just . func) . findAttrValueByName attrName

parseChildrenWith
  :: (Element -> Either Error a) -> String -> Element -> Either Error [a]
parseChildrenWith func childName elem =
  forM (findChildrenByName childName elem) func

parseDate :: String -> Either Error Day
parseDate =
  fmap (localDay . fst) . justErr "Incorrect date format" . strptime "%Y-%m-%d"

parseDateTime :: String -> Either Error LocalTime
parseDateTime =
  fmap fst . justErr "Incorrect dateTime format" . strptime "%Y-%m-%dT%H:%M:%S"

parseElementWith
  :: (Element -> Either Error a) -> String -> Element -> Either Error (Maybe a)
parseElementWith func elemName =
  maybe (Right Nothing) (fmap Just . func) . findChildByName elemName

requireAndParseAttrWith
  :: (String -> Either Error a) -> String -> Element -> Either Error a
requireAndParseAttrWith func attrName elem =
  requireAttrValueByName attrName elem >>= func

requireAndParseChildWith
  :: (Element -> Either Error a) -> String -> Element -> Either Error a
requireAndParseChildWith func childName parent =
  requireChildByName childName parent >>= func

requireAndReadAttribute :: Read r => String -> Element -> Either Error r
requireAndReadAttribute attrName =
  fmap read . requireAttrValueByName attrName

requireAttrValueByName :: String -> Element -> Either Error String
requireAttrValueByName attrName =
  justErr (attrName ++ " attribute not found") . findAttrValueByName attrName

requireChildByName :: String -> Element -> Either Error Element
requireChildByName childName =
  justErr (childName ++ " element not found") . findChildByName childName
