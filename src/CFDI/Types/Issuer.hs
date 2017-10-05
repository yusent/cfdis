module CFDI.Types.Issuer where

import CFDI.Types.Name
import CFDI.Types.RFC
import CFDI.Types.TaxRegime
import CFDI.XmlNode
import Data.Maybe           (catMaybes)

data Issuer = Issuer
  { issName :: Maybe Name
  , issRfc  :: RFC
  , taxReg  :: TaxRegime
  } deriving (Eq, Show)

instance XmlNode Issuer where
  attributes n =
    [ attr "Rfc"           $ issRfc n
    , attr "RegimenFiscal" $ taxReg n
    ] ++ catMaybes
    [ attr "Nombre" <$> issName n
    ]

  nodeName = const "Emisor"

  parseNode n = Issuer
    <$> parseAttribute "Nombre" n
    <*> requireAttribute "Rfc" n
    <*> requireAttribute "RegimenFiscal" n
