module CFDI.XmlNode where

import CFDI.Types.Type      (ParseError, Type, parse, render)
import Control.Error.Safe   (justErr)
import Text.XML.Light.Proc  (filterChildName, filterChildrenName, findAttrBy)
import Text.XML.Light
  ( Attr(..)
  , Content(Elem)
  , Element(..)
  , QName(..)
  )

data XmlParseError
  = AttrNotFound String
  | AttrParseError String ParseError
  | ElemNotFound String
  | ExpectedAtLeastOne String
  | MalformedXML
  | ParseErrorInChild String XmlParseError
  deriving (Eq, Show)

class XmlNode n where
  attributes :: n -> [Attr]
  attributes = const []

  children :: n -> [Element]
  children = const []

  nodeName :: n -> String

  nodePrefix :: n -> String
  nodePrefix = const "cfdi"

  parseNode :: Element -> Either XmlParseError n

  renderNode :: n -> Element
  renderNode n = mkElem (nodePrefix n) (nodeName n) (attributes n) (children n)

-- Helpers

attr :: Type t => String -> t -> Attr
attr attrName = Attr (QName attrName Nothing Nothing) . render

attrWithPrefix :: Type t => String -> String -> t -> Attr
attrWithPrefix prefix attrName =
  Attr (QName attrName Nothing (Just prefix)) . render

findAttrValueByName :: String -> Element -> Maybe String
findAttrValueByName attrName = findAttrBy (nameEquals attrName)

findChildByName :: String -> Element -> Maybe Element
findChildByName childName = filterChildName (nameEquals childName)

findChildrenByName :: String -> Element -> [Element]
findChildrenByName childName = filterChildrenName (nameEquals childName)

mkElem :: String -> String -> [Attr] -> [Element] -> Element
mkElem prefix elemName attrs elems = Element
  (QName elemName Nothing $ Just prefix)
  attrs
  (Elem <$> elems)
  Nothing

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
