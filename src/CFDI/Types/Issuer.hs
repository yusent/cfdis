module CFDI.Types.Issuer where

import CFDI.Types.Name
import CFDI.Types.RFC
import CFDI.Types.TaxRegime
import CFDI.XmlNode

data Issuer = Issuer
  { issName :: Maybe Name
  , issRfc  :: RFC
  , taxReg  :: TaxRegime
  } deriving (Eq, Show)

instance XmlNode Issuer where
  parseNode n = Issuer
    <$> parseAttribute "Nombre" n
    <*> requireAttribute "Rfc" n
    <*> requireAttribute "RegimenFiscal" n
