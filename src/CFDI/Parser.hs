module CFDI.Parser where

import CFDI.Types.CFDI      (CFDI)
import CFDI.XmlNode         (XmlParseError(MalformedXML), parseNode)
import Control.Error.Safe   (justErr)
import Text.XML.Light       (parseXMLDoc)
import Text.XML.Light.Lexer (XmlSource)

parseCfdiXml :: XmlSource s => s -> Either XmlParseError CFDI
parseCfdiXml xmlSource = justErr MalformedXML (parseXMLDoc xmlSource)
                     >>= parseNode

parseCfdiFile :: FilePath -> IO (Either XmlParseError CFDI)
parseCfdiFile fp = parseCfdiXml <$> readFile fp
