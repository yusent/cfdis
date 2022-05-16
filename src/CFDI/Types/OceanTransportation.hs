module CFDI.Types.OceanTransportation where

import CFDI.Chainable
import CFDI.XmlNode
import CFDI.Types.Description3_30
import CFDI.Types.Description3_50
import CFDI.Types.Description50
import CFDI.Types.Description7_30
import CFDI.Types.SCTPermissionType
import CFDI.Types.VesselType
import Data.Maybe (catMaybes)

data OceanTransportation = OceanTransportation
  { octrSCTPermType :: SCTPermissionType
  , octrSCTPermNum :: Description50
  , octrInsuranceName :: Description3_50
  , octrInsurancePolicy :: Description3_30
  , octrVesselType :: VesselType
  , octrPlate :: Description7_30
  } deriving (Eq, Show)

instance Chainable OceanTransportation where
  chain _ = ""

instance XmlNode OceanTransportation where
  attributes n = catMaybes
    [ attr "PermSCT" <$> octrSCTPermType n
    , attr "NumPermisoSCT" <$> octrSCTPermNum n
    , attr "NombreAseg" <$> octrInsuranceName n
    , attr "NumPolizaSeguro" <$> octrInsurancePolicy n
    , Just . attr "TipoEmbarcacion" $ octrVesselType n
    , Just . attr "Matricula" $ octrPlate n
    ]

  children n = catMaybes
    [
    ]

  nodeName = const "TransporteMaritimo"

  parseNode n = OceanTransportation
    <$> parseAttribute "PermSCT" n
    <*> parseAttribute "NumPermisoSCT" n
    <*> parseAttribute "NombreAseg" n
    <*> parseAttribute "NumPolizaSeguro" n
    <*> requireAttribute "TipoEmbarcacion" n
    <*> requireAttribute "Matricula" n
