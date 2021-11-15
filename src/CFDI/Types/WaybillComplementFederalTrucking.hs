module CFDI.Types.WaybillComplementFederalTrucking where

import CFDI.Chainable
import CFDI.XmlNode
import CFDI.Types.Description3_30
import CFDI.Types.Description3_50
import CFDI.Types.Description50
import CFDI.Types.SCTPermissionType
import Data.Maybe (catMaybes)

data WaybillComplementFederalTrucking = WaybillComplementFederalTrucking
  { wcftSCTPermType :: SCTPermissionType
  , wcftSCTPermNum :: Description50
  , wcftInsuranceName :: Description3_50
  , wcftInsurancePolicy :: Description3_30
  } deriving (Eq, Show)

instance Chainable WaybillComplementFederalTrucking where
  chain _ = ""

instance XmlNode WaybillComplementFederalTrucking where
  attributes n =
    [ attr "PermSCT" $ wcftSCTPermType n
    , attr "NumPermisoSCT" $ wcftSCTPermNum n
    , attr "NombreAseg" $ wcftInsuranceName n
    , attr "NumPolizaSeguro" $ wcftInsurancePolicy n
    ]

  children n = catMaybes
    [
    ]

  nodeName = const "AutotransporteFederal"

  parseNode n = WaybillComplementFederalTrucking
    <$> requireAttribute "PermSCT" n
    <*> requireAttribute "NumPermisoSCT" n
    <*> requireAttribute "NombreAseg" n
    <*> requireAttribute "NumPolizaSeguro" n
