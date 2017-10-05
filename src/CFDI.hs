module CFDI
  ( module CFDI.CSD
  , module CFDI.Types
  , module XN
  , addCsdCerData
  , parseCfdiFile
  , parseCfdiXml
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

toXML :: CFDI -> String
toXML = ppTopElement . renderNode
