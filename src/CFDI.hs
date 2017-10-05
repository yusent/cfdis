module CFDI
  ( module CFDI.CSD
  , module CFDI.Types
  , module XN
  , addCsdCerData
  , originalChain
  , parseCfdiFile
  , parseCfdiXml
  , ppXmlParseError
  , toXML
  ) where

import CFDI.Chainable       (chain)
import CFDI.CSD             (CsdCerData(..))
import CFDI.Types
import CFDI.XmlNode         (parseNode, renderNode)
import CFDI.XmlNode as XN   (XmlParseError(..))
import Control.Error.Safe   (justErr)
import Data.List            (intersperse)
import Data.Text            (Text, append)
import Text.XML.Light       (parseXMLDoc, ppTopElement)
import Text.XML.Light.Lexer (XmlSource)

addCsdCerData :: CsdCerData -> CFDI -> CFDI
addCsdCerData CsdCerData { cerNumber = cn, cerToText = ct } cfdi =
  cfdi { certNum = Just (CertificateNumber cn), certText = Just ct }

originalChain :: CFDI -> Text
originalChain cfdi = "||" `append` chain cfdi `append` "||"

parseCfdiFile :: FilePath -> IO (Either XmlParseError CFDI)
parseCfdiFile fp = parseCfdiXml <$> readFile fp

parseCfdiXml :: XmlSource s => s -> Either XmlParseError CFDI
parseCfdiXml xmlSource = justErr MalformedXML (parseXMLDoc xmlSource)
                     >>= parseNode

ppParseError :: ParseError -> String
ppParseError (InvalidValue v) =
  '"' : v ++ "\" no es un valor válido para este atributo."
ppParseError (DoesNotMatchExpr e) =
  "No cumple con la expresión \"" ++ e ++ "\"."
ppParseError NotInCatalog =
  "No se encuentra en el catálogo de valores permitidos publicado por el SAT."

ppXmlParseError :: String -> XmlParseError -> String
ppXmlParseError indentationStr = concat
  . intersperse "\n"
  . fst
  . foldl addIndentation ([], "")
  . errMsgLines
  where
    addIndentation (ls, s) l = (ls ++ [s ++ l], s ++ indentationStr)
    errMsgLines (AttrNotFound a) =
      ["No se encontró el atributo \"" ++ a ++ "\"."]
    errMsgLines (AttrParseError a pe) =
      ["No se pudo interpretar el atributo \"" ++ a ++ "\":", ppParseError pe]
    errMsgLines (ElemNotFound e) =
      ["No se encontró el elemento \"" ++ e ++ "\"."]
    errMsgLines (ExpectedAtLeastOne e) =
      ["Se necesita al menos un \"" ++ e ++ "\"."]
    errMsgLines MalformedXML =
      ["XML malformado o inválido."]
    errMsgLines (ParseErrorInChild e xpe) =
      ("Se encontró un error en el elemento \"" ++ e ++ "\":") : errMsgLines xpe

toXML :: CFDI -> String
toXML = ppTopElement . renderNode
