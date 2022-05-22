module CFDI.Types.Recipient where

import CFDI.Chainable
import CFDI.Types.Country
import CFDI.Types.Name
import CFDI.Types.RFC
import CFDI.Types.TaxId
import CFDI.Types.TaxRegime
import CFDI.Types.Use
import CFDI.Types.ZipCode
import CFDI.XmlNode
import Data.Maybe         (catMaybes)

data Recipient = Recipient
  { recCfdiUse   :: Maybe Use
  , recName      :: Maybe Name
  , recRfc       :: RFC
  , recTaxId     :: Maybe TaxId
  , taxResidence :: Maybe Country
  , taxAddress   :: Maybe ZipCode
  , recTaxReg    :: Maybe TaxRegime
  } deriving (Eq, Show)

instance Chainable Recipient where
  chain c = recRfc
        <@> recName
        <~> taxResidence
        <~> recTaxId
        <~> recCfdiUse
        <~> (c, "")

instance XmlNode Recipient where
  attributes n = attr "Rfc" (recRfc n)
    : catMaybes
      [ attr "UsoCFDI"                 <$> recCfdiUse n
      , attr "Nombre"                  <$> recName n
      , attr "ResidenciaFiscal"        <$> taxResidence n
      , attr "NumRegIdTrib"            <$> recTaxId n
      , attr "DomicilioFiscalReceptor" <$> taxAddress n
      , attr "RegimenFiscalReceptor"   <$> recTaxReg n
      ]

  nodeName = const "Receptor"

  parseNode n = Recipient
    <$> parseAttribute "UsoCFDI" n
    <*> parseAttribute "Nombre" n
    <*> requireAttribute "Rfc" n
    <*> parseAttribute "NumRegIdTrib" n
    <*> parseAttribute "ResidenciaFiscal" n
    <*> parseAttribute "DomicilioFiscalReceptor" n
    <*> parseAttribute "RegimenFiscalReceptor" n
