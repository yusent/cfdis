module CFDI
  ( module CFDI.Types.CFDI
  , module CFDI.Types.Type
  , module XN
  , parseCfdiFile
  , parseCfdiXml
  , toXML
  ) where

import CFDI.Types.CFDI      (CFDI)
import CFDI.Types.Type      (ParseError(..))
import CFDI.XmlNode         (parseNode, renderNode)
import CFDI.XmlNode as XN   (XmlParseError(..))
import Control.Error.Safe   (justErr)
import Text.XML.Light       (parseXMLDoc, ppTopElement)
import Text.XML.Light.Lexer (XmlSource)

parseCfdiFile :: FilePath -> IO (Either XmlParseError CFDI)
parseCfdiFile fp = parseCfdiXml <$> readFile fp

parseCfdiXml :: XmlSource s => s -> Either XmlParseError CFDI
parseCfdiXml xmlSource = justErr MalformedXML (parseXMLDoc xmlSource)
                     >>= parseNode

toXML :: CFDI -> String
toXML = ppTopElement . renderNode
