module CFDI.Types.WaybillComplement where

import CFDI.Chainable
import CFDI.Types.InOut
import CFDI.Types.InOutWay
import CFDI.Types.WaybillComplementGoods
import CFDI.Types.WaybillComplementLocations
import CFDI.Types.WaybillComplementVersion
import CFDI.Types.YesNo
import CFDI.XmlNode
import Data.Maybe (catMaybes)
import Data.Text (Text, append, intercalate)

data WaybillComplement = WaybillComplement
  { waybillComplementVersion :: WaybillComplementVersion
  , internationalTransportation :: YesNo
  , inventoryInOrOut :: Maybe InOut
  , inventoryInOrOutWay :: Maybe InOutWay
  , distanceTraveled :: Maybe Rational
  , waybillComplementLocations :: WaybillComplementLocations
  , waybillComplementGoods :: WaybillComplementGoods
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
    , attr "ViaEntradaSalida" <$> inventoryInOrOutWay n
    , attr "TotalDistRec" <$> distanceTraveled n
    ]

  children complement =
    [ renderNode $ waybillComplementLocations complement
    , renderNode $ waybillComplementGoods complement
    ]

  nodeName = const "CartaPorte"

  nodePrefix = const "cartaporte"

  parseNode n = WaybillComplement
    <$> requireAttribute "Version" n
    <*> requireAttribute "TranspInternac" n
    <*> parseAttribute "EntradaSalidaMerc" n
    <*> parseAttribute "ViaEntradaSalida" n
    <*> parseAttribute "TotalDistRec" n
    <*> requireChild "Ubicaciones" n
    <*> requireChild "Mercancias" n
