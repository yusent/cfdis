module CFDI.Parser (parseCFDI) where

import Data.Maybe           (fromJust, fromMaybe)
import Data.Time.LocalTime  (LocalTime)
import Data.Time.Parse      (strptime)
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Lexer (XmlSource)
import Text.XML.Light.Proc  (findAttrBy)
import Text.XML.Light.Types (Element(Element), QName(QName))
import CFDI.Types

findAttrValueByName :: String -> Element -> Maybe String
findAttrValueByName attrName element =
  findAttrBy (\(QName a _ _) -> a == attrName) element

parseCFDI :: XmlSource s => s -> Maybe CFDI
parseCFDI xmlSource =
  parseCFDIv3_2 <$> parseXMLDoc xmlSource

parseCFDIv3_2 :: Element -> CFDI
parseCFDIv3_2 root =
  CFDI {
    accountNumber = findAttrValueByName "NumCtaPago" root,
    certificate = requireAttrValueByName "certificado" root,
    certificateNumber = requireAttrValueByName "noCertificado" root,
    currency = findAttrValueByName "Moneda" root,
    expeditionPlace = requireAttrValueByName "LugarExpedicion" root,
    internalID = findAttrValueByName "folio" root,
    issuedAt = parseDateTime $ requireAttrValueByName "fecha" root,
    paymentConditions = findAttrValueByName "condicionesDePago" root,
    paymentMethod = requireAttrValueByName "metodoDePago" root,
    subTotal = read $ requireAttrValueByName "subTotal" root,
    signature = fromMaybe "" $ findAttrValueByName "sello" root,
    total = read $ requireAttrValueByName "total" root,
    _type = requireAttrValueByName "tipoDeComprobante" root,
    version = requireAttrValueByName "version" root
  }

parseDateTime :: String -> LocalTime
parseDateTime = fst . fromJust . strptime "%Y-%m-%dT%H:%M:%S"

requireAttrValueByName :: String -> Element -> String
requireAttrValueByName attrName = fromJust . findAttrValueByName attrName
