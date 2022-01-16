module CFDI.Types.VehicleID where

import CFDI.Chainable
import CFDI.Types.VehicleConfig
import CFDI.Types.VehicleModelYear
import CFDI.Types.VehiclePlate
import CFDI.XmlNode

data VehicleID = VehicleID
  { viVehicleConfig :: VehicleConfig
  , viPlate :: VehiclePlate
  , viModelYear :: VehicleModelYear
  } deriving (Eq, Show)

instance Chainable VehicleID where
  chain _ = ""

instance XmlNode VehicleID where
  attributes n =
    [ attr "ConfigVehicular" $ viVehicleConfig n
    , attr "PlacaVM" $ viPlate n
    , attr "AnioModeloVM" $ viModelYear n
    ]

  nodeName = const "IdentificacionVehicular"

  parseNode n = VehicleID
    <$> requireAttribute "ConfigVehicular" n
    <*> requireAttribute "PlacaVM" n
    <*> requireAttribute "AnioModeloVM" n
