module CFDI.XmlNode where

import CFDI.Types.Type      (ParseError, Type, parse)
import Control.Error.Safe   (justErr)
import Text.XML.Light.Proc  (findAttrBy)
import Text.XML.Light.Types (Element, QName(QName))

data XmlParseError
  = AttrNotFound String
  | AttrParseError String ParseError
  | ElemNotFound String
  | MalformedXML
  | ParseErrorInChild String XmlParseError
  deriving (Eq, Show)

class XmlNode n where
  parseNode :: Element -> Either XmlParseError n

-- Helpers

findAttrValueByName :: String -> Element -> Maybe String
findAttrValueByName attrName = findAttrBy (nameEquals attrName)

nameEquals :: String -> QName -> Bool
nameEquals s (QName name _ _) = s == name

parseAttribute :: Type t => String -> Element -> Either XmlParseError (Maybe t)
parseAttribute attrName el =
  case findAttrValueByName attrName el of
    Just str ->
      case parse str of
        Left err -> Left $ AttrParseError attrName err
        Right t  -> Right $ Just t
    Nothing  ->
      Right Nothing

requireAttribute :: Type t => String -> Element -> Either XmlParseError t
requireAttribute attrName el = do
  val <- justErr (AttrNotFound attrName) $ findAttrValueByName attrName el
  case parse val of
    Left err -> Left $ AttrParseError attrName err
    Right pv -> Right pv
