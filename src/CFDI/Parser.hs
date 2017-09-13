module CFDI.Parser (parseCFDI) where

import CFDI
import Control.Error.Safe   (justErr)
import Control.Monad        (forM, sequence)
import Data.Maybe           (fromMaybe)
import Data.Time.Calendar   (Day)
import Data.Time.LocalTime  (LocalTime, localDay)
import Data.Time.Parse      (strptime)
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Lexer (XmlSource)
import Text.XML.Light.Proc  (filterElementName, filterElementsName, findAttrBy)
import Text.XML.Light.Types (Element(Element), QName(QName))

data ParseError
  = AttrNotFound
      { attrName :: String
      }
  | ElemNotFound
      { elemName :: String
      }
  | InvalidFormat
      { formattedName :: String
      }
  | MalformedXML
  | ParseErrorInChild
      { parsedName :: String
      , childErr   :: ParseError
      }
  deriving (Eq, Show)

type Parsed = Either ParseError

-- Main functions

parseCFDI :: XmlSource s => s -> Parsed CFDI
parseCFDI xmlSource =
  justErr MalformedXML (parseXMLDoc xmlSource) >>= parseCFDIv3_2

parseCFDIv3_2 :: Element -> Parsed CFDI
parseCFDIv3_2 root = CFDI
  <$> parseAttribute "NumCtaPago" root
  <*> requireAttrValueByName "certificado" root
  <*> requireAttrValueByName "noCertificado" root
  <*> parseChildWith parseComplement "Complemento" root
  <*> do
    conceptsNode <- requireChildByName "Conceptos" root
    parseChildrenWith parseConcept "Concepto" conceptsNode
  <*> parseAttribute "Moneda" root
  <*> parseAttribute "descuento" root
  <*> parseAttribute "motivoDescuento" root
  <*> parseAttribute "TipoCambio" root
  <*> parseAttribute "folio" root
  <*> requireAndParseAttrWith parseDateTime "fecha" root
  <*> requireAttrValueByName "LugarExpedicion" root
  <*> requireAndParseChildWith parseIssuer "Emisor" root
  <*> parseAttribute "MontoFolioFiscalOrig" root
  <*> parseAttributeWith parseDateTime "FechaFolioFiscalOrig" root
  <*> parseAttribute "FolioFiscalOrig" root
  <*> parseAttribute "SerieFolioFiscalOrig" root
  <*> parseAttribute "condicionesDePago" root
  <*> requireAttrValueByName "metodoDePago" root
  <*> requireAndParseChildWith parseRecipient "Receptor" root
  <*> parseAttribute "serie" root
  <*> requireAttrValueByName "subTotal" root
  <*> Right (fromMaybe "" $ findAttrValueByName "sello" root)
  <*> requireAndParseChildWith parseTaxes "Impuestos" root
  <*> requireAttrValueByName "total" root
  <*> requireAttrValueByName "tipoDeComprobante" root
  <*> requireAttrValueByName "version" root
  <*> requireAttrValueByName "formaDePago" root

-- Parsers

parseAddress :: Element -> Parsed Address
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

parseComplement :: Element -> Parsed Complement
parseComplement element = Complement
  <$> parseChildWith parsePacStamp "TimbreFiscalDigital" element

parseConcept :: Element -> Parsed Concept
parseConcept element = Concept
  <$> requireAttrValueByName "importe" element
  <*> requireAttrValueByName "descripcion" element
  <*> parseAttribute "noIdentificacion" element
  <*> parseChildrenWith parseImportInfo "InformacionAduanera" element
  <*> parseChildrenWith parseConceptPart "Parte" element
  <*> parseChildWith parsePropertyAccount "CuentaPredial" element
  <*> requireAttrValueByName "cantidad" element
  <*> requireAttrValueByName "unidad" element
  <*> requireAttrValueByName "valorUnitario" element

parseConceptPart :: Element -> Parsed ConceptPart
parseConceptPart element = ConceptPart
  <$> parseAttribute "importe" element
  <*> requireAttrValueByName "descripcion" element
  <*> parseAttribute "noIdentificacion" element
  <*> parseChildrenWith parseImportInfo "InformacionAduanera" element
  <*> requireAttrValueByName "cantidad" element
  <*> parseAttribute "unidad" element
  <*> parseAttribute "valorUnitario" element

parseFiscalAddress :: Element -> Parsed FiscalAddress
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

parseImportInfo :: Element -> Parsed ImportInfo
parseImportInfo element = ImportInfo
  <$> parseAttribute "aduana" element
  <*> requireAndParseAttrWith parseDate "fecha" element
  <*> requireAttrValueByName "numero" element

parseIssuer :: Element -> Parsed Issuer
parseIssuer element = Issuer
  <$> parseChildWith parseFiscalAddress "DomicilioFiscal" element
  <*> parseChildWith parseAddress "ExpedidoEn" element
  <*> parseAttribute "nombre" element
  <*> parseChildrenWith parseTaxRegime "RegimenFiscal" element
  <*> requireAttrValueByName "rfc" element

parsePacStamp :: Element -> Parsed PacStamp
parsePacStamp element = PacStamp
  <$> requireAttrValueByName "selloCFD" element
  <*> requireAttrValueByName "noCertificadoSAT" element
  <*> requireAttrValueByName "selloSAT" element
  <*> requireAndParseAttrWith parseDateTime "FechaTimbrado" element
  <*> requireAttrValueByName "version" element
  <*> requireAttrValueByName "UUID" element

parsePropertyAccount :: Element -> Parsed PropertyAccount
parsePropertyAccount element = PropertyAccount
  <$> requireAttrValueByName "numero" element

parseRecipient :: Element -> Parsed Recipient
parseRecipient element = Recipient
  <$> parseChildWith parseAddress "Domicilio" element
  <*> parseAttribute "nombre" element
  <*> requireAttrValueByName "rfc" element

parseRetainedTax :: Element -> Parsed RetainedTax
parseRetainedTax element = RetainedTax
  <$> requireAttrValueByName "importe" element
  <*> requireAndReadAttribute "impuesto" element

parseTaxes :: Element -> Parsed Taxes
parseTaxes element = Taxes
  <$> sequence rt
  <*> sequence tt
  <*> parseAttribute "totalImpuestosRetenidos" element
  <*> parseAttribute "totalImpuestosTrasladados" element
  where
    rt = maybe [] (map parseRetainedTax)
       . fmap (findChildrenByName "Retencion")
       $ findChildByName "Retenciones" element
    tt = maybe [] (map parseTransferedTax)
       . fmap (findChildrenByName "Traslado")
       $ findChildByName "Traslados" element

parseTaxRegime :: Element -> Parsed TaxRegime
parseTaxRegime element = TaxRegime
  <$> requireAttrValueByName "Regimen" element

parseTransferedTax :: Element -> Parsed TransferedTax
parseTransferedTax element = TransferedTax
  <$> requireAttrValueByName "importe" element
  <*> requireAttrValueByName "tasa" element
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

parseAndReadAttribute :: Read r => String -> Element -> Parsed (Maybe r)
parseAndReadAttribute attrName =
  Right . fmap read . findAttrValueByName attrName

parseAttribute :: String -> Element -> Parsed (Maybe String)
parseAttribute attrName =
  Right . findAttrValueByName attrName

parseAttributeWith :: (String -> Maybe a) -> String -> Element -> Parsed (Maybe a)
parseAttributeWith f attrName elem =
  case findAttrValueByName attrName elem of
    Nothing -> Right Nothing
    Just x  -> Just <$> justErr (InvalidFormat attrName) (f x)

parseChildrenWith :: (Element -> Parsed a) -> String -> Element -> Parsed [a]
parseChildrenWith parserFunction childName parent =
  forM children parseOrErr
  where
    children   = findChildrenByName childName parent
    parseOrErr = wrapError (ParseErrorInChild childName) . parserFunction

parseChildWith :: (Element -> Parsed a) -> String -> Element -> Parsed (Maybe a)
parseChildWith f childName parent =
  case findChildByName childName parent of
    Nothing -> Right Nothing
    Just x  -> Just <$> wrapError (ParseErrorInChild childName) (f x)

parseDate :: String -> Maybe Day
parseDate =
  fmap (localDay . fst) . strptime "%Y-%m-%d"

parseDateTime :: String -> Maybe LocalTime
parseDateTime =
  fmap fst . strptime "%Y-%m-%dT%H:%M:%S"

requireAndParseAttrWith :: (String -> Maybe a) -> String -> Element -> Parsed a
requireAndParseAttrWith f attrName elem =
  requireAttrValueByName attrName elem >>= justErr (InvalidFormat attrName) . f

requireAndParseChildWith :: (Element -> Parsed a) -> String -> Element -> Parsed a
requireAndParseChildWith f childName parent =
  requireChildByName childName parent >>= wrapError (ParseErrorInChild childName) . f

requireAndReadAttribute :: Read r => String -> Element -> Parsed r
requireAndReadAttribute attrName =
  fmap read . requireAttrValueByName attrName

requireAttrValueByName :: String -> Element -> Parsed String
requireAttrValueByName attrName =
  justErr (AttrNotFound attrName) . findAttrValueByName attrName

requireChildByName :: String -> Element -> Parsed Element
requireChildByName childName =
  justErr (ElemNotFound childName) . findChildByName childName

wrapError :: (ParseError -> ParseError) -> Parsed a -> Parsed a
wrapError f (Left err) = Left $ f err
wrapError _ x          = x
