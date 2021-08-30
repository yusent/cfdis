module CFDI.Types.WaybillComplement where

import CFDI.Chainable
import CFDI.Types.InOut
import CFDI.Types.WaybillComplementVersion
import CFDI.Types.YesNo
import CFDI.XmlNode
import Data.Maybe (catMaybes)
import Data.Text (Text, append, intercalate)

data WaybillComplement = WaybillComplement
  { waybillComplementVersion :: WaybillComplementVersion
  , internationalTransportation :: YesNo
  , inventoryInOrOut :: Maybe InOut
  } deriving (Eq, Show)

instance Chainable WaybillComplement where
  chain = chain . waybillComplementVersion

instance XmlNode WaybillComplement where
  attributes n =
    [ attrWithPrefix "xsi" "schemaLocation"
        ("http://www.sat.gob.mx/CartaPorte \
         \http://www.sat.gob.mx/sitio_internet/cfd/CartaPorte/CartaPorte.xsd" :: Text)
    , attrWithPrefix "xmlns" "cartaporte" ("http://www.sat.gob.mx/CartaPorte" :: Text)
    , attr "Version" $ waybillComplementVersion n
    , attr "TranspInternac" $ internationalTransportation n
    ] ++ catMaybes
    [ attr "EntradaSalidaMerc" <$> inventoryInOrOut n
    ]

  children (WaybillComplement _ _ _) = []

  nodeName = const "CartaPorte"

  nodePrefix = const "cartaporte"

  parseNode n = WaybillComplement
    <$> requireAttribute "Version" n
    <*> requireAttribute "TranspInternac" n
    <*> parseAttribute "EntradaSalidaMerc" n
