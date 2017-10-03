module CFDI.XmlNode where

import CFDI.Types.Type      (ParseError, Type, parse)
import Control.Error.Safe   (justErr)
import Text.XML.Light.Proc  (filterChildName, filterChildrenName, findAttrBy)
import Text.XML.Light.Types (Element, QName(QName))

data XmlParseError
  = AttrNotFound String
  | AttrParseError String ParseError
  | ElemNotFound String
  | ExpectedAtLeastOne String
  | MalformedXML
  | ParseErrorInChild String XmlParseError
  deriving (Eq, Show)

class XmlNode n where
  parseNode :: Element -> Either XmlParseError n

-- Helpers

findAttrValueByName :: String -> Element -> Maybe String
findAttrValueByName attrName = findAttrBy (nameEquals attrName)

findChildByName :: String -> Element -> Maybe Element
findChildByName childName = filterChildName (nameEquals childName)

findChildrenByName :: String -> Element -> [Element]
findChildrenByName childName = filterChildrenName (nameEquals childName)

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

parseChild :: XmlNode n => String -> Element -> Either XmlParseError (Maybe n)
parseChild childName parent =
  case findChildByName childName parent of
    Nothing -> Right Nothing
    Just n  ->
      case parseNode n of
        Left  err -> Left  $ ParseErrorInChild childName err
        Right pn  -> Right $ Just pn

parseChildren :: XmlNode n => String -> Element -> Either XmlParseError [n]
parseChildren childName = mapM parseNode' . findChildrenByName childName
  where
    parseNode' n = case parseNode n of
      Left  err -> Left  $ ParseErrorInChild childName err
      Right mpn -> Right mpn

requireAttribute :: Type t => String -> Element -> Either XmlParseError t
requireAttribute attrName el = do
  val <- justErr (AttrNotFound attrName) $ findAttrValueByName attrName el
  case parse val of
    Left err -> Left $ AttrParseError attrName err
    Right pv -> Right pv

requireChild :: XmlNode n => String -> Element -> Either XmlParseError n
requireChild childName parent = do
  node <- justErr (ElemNotFound childName) $ findChildByName childName parent
  case parseNode node of
    Left err -> Left $ ParseErrorInChild childName err
    Right pn -> Right pn
