module CFDI.Types.Trailer where

import CFDI.Chainable
import CFDI.XmlNode
import CFDI.Types.TrailerSubType
import CFDI.Types.VehiclePlate

data Trailer = Trailer
  { trSubType :: TrailerSubType
  , trPlate :: VehiclePlate
  } deriving (Eq, Show)

instance Chainable Trailer where
  chain _ = ""

instance XmlNode Trailer where
  attributes n =
    [ attr "SubTipoRem" $ trSubType n
    , attr "Placa" $ trPlate n
    ]

  nodeName = const "Remolque"

  parseNode n = Trailer
    <$> requireAttribute "SubTipoRem" n
    <*> requireAttribute "Placa" n
