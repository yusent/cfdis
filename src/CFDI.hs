module CFDI
  ( module CFDI.Types.CFDI
  , parseCfdiFile
  , parseCfdiXml
  , toXML
  ) where

import CFDI.Types.CFDI      (CFDI)
import CFDI.XmlNode         (XmlParseError(MalformedXML), parseNode, renderNode)
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
