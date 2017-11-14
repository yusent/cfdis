module CFDI
  ( module CFDI.Types
  , module CSD
  , module XN
  , addCsdCerData
  , findAddendumByName
  , getStampComplement
  , originalChain
  , parseCfdiFile
  , parseCfdiXml
  , ppXmlParseError
  , signWith
  , toXML
  ) where

import CFDI.Chainable       (chain)
import CFDI.CSD             (signWithCSD)
import CFDI.CSD as CSD      (CsdCerData(..))
import CFDI.Types
import CFDI.XmlNode         (parseNode, renderNode)
import CFDI.XmlNode as XN   (XmlParseError(..))
import Control.Error.Safe   (justErr)
import Data.List            (find, intersperse)
import Data.Text            (Text, append)
import Text.XML.Light       (Element(..), QName(..), parseXMLDoc, ppTopElement)
import Text.XML.Light.Lexer (XmlSource)

addCsdCerData :: CsdCerData -> CFDI -> CFDI
addCsdCerData CsdCerData { cerNumber = cn, cerToText = ct } cfdi =
  cfdi { certNum = Just (CertificateNumber cn), certText = Just ct }

findAddendumByName :: String -> CFDI -> Maybe Element
findAddendumByName _ CFDI { addenda = Nothing } = Nothing
findAddendumByName name CFDI { addenda = Just (Addenda xs) } = find match xs
  where
    match Element { elName = QName n _ _ } = n == name

getStampComplement :: CFDI -> Maybe Complement
getStampComplement CFDI { complement = comps } = find isStampComplement comps
  where
    isStampComplement (StampComplement _) = True
    isStampComplement _ = False

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
    errMsgLines UnknownComplement =
      ["Complemento desconocido."]

signWith :: FilePath -> CFDI -> IO (Either Text CFDI)
signWith csdPemPath cfdi =
  fmap (fmap addSignatureToCFDI) . signWithCSD csdPemPath $ originalChain cfdi
  where
    addSignatureToCFDI sig = cfdi { signature = Just sig }

toXML :: CFDI -> String
toXML = ppTopElement . renderNode
