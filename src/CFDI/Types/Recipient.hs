module CFDI.Types.Recipient where

import CFDI.Types.Country
import CFDI.Types.Name
import CFDI.Types.RFC
import CFDI.Types.TaxId
import CFDI.Types.Use
import CFDI.XmlNode

data Recipient = Recipient
  { recCfdiUse   :: Maybe Use
  , recName      :: Maybe Name
  , recRfc       :: RFC
  , recTaxId     :: Maybe TaxId
  , taxResidence :: Maybe Country
  } deriving (Eq, Show)

instance XmlNode Recipient where
  parseNode n = Recipient
    <$> parseAttribute "UsoCFDI" n
    <*> parseAttribute "Nombre" n
    <*> requireAttribute "Rfc" n
    <*> parseAttribute "NumRegIdTrib" n
    <*> parseAttribute "ResidenciaFiscal" n
