module CFDI
  ( module CFDI.CSD
  , module CFDI.Types
  , module XN
  , addCsdCerData
  , parseCfdiFile
  , parseCfdiXml
  , ppXmlParseError
  , toXML
  ) where

import CFDI.CSD             (CsdCerData(..))
import CFDI.Types
import CFDI.XmlNode         (parseNode, renderNode)
import CFDI.XmlNode as XN   (XmlParseError(..))
import Control.Error.Safe   (justErr)
import Text.XML.Light       (parseXMLDoc, ppTopElement)
import Text.XML.Light.Lexer (XmlSource)

addCsdCerData :: CsdCerData -> CFDI -> CFDI
addCsdCerData CsdCerData { cerNumber = cn, cerToText = ct } cfdi =
  cfdi { certNum = Just (CertificateNumber cn), certText = Just ct }

parseCfdiFile :: FilePath -> IO (Either XmlParseError CFDI)
parseCfdiFile fp = parseCfdiXml <$> readFile fp

parseCfdiXml :: XmlSource s => s -> Either XmlParseError CFDI
parseCfdiXml xmlSource = justErr MalformedXML (parseXMLDoc xmlSource)
                     >>= parseNode

ppParseError :: ParseError -> String
ppParseError (InvalidValue v) =
  '"' : v ++ "\" no es un valor válido para este atributo."
ppParseError (DoesNotMatchExpr e) =
  "no cumple con la expresión \"" ++ e ++ "\"."
ppParseError NotInCatalog =
  "no se encuentra en el catálogo de valores permitidos publicado por el SAT."

ppXmlParseError :: XmlParseError -> String
ppXmlParseError (AttrNotFound a) =
  "No se encontró el atributo \"" ++ a ++ "\"."
ppXmlParseError (AttrParseError a pe) =
  "No se pudo interpretar el atributo \"" ++ a ++ "\": " ++ ppParseError pe
ppXmlParseError (ElemNotFound e) =
  "No se encontró el elemento \"" ++ e ++ "\"."
ppXmlParseError (ExpectedAtLeastOne e) =
  "Se necesita al menos un \"" ++ e ++ "\"."
ppXmlParseError MalformedXML =
  "XML malformado o inválido."
ppXmlParseError (ParseErrorInChild e xpe) =
  "Se encontró un error en el elemento \"" ++ e ++ "\": " ++ ppXmlParseError xpe

toXML :: CFDI -> String
toXML = ppTopElement . renderNode
