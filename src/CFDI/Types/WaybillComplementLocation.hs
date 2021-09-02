module CFDI.Types.WaybillComplementLocation where

import CFDI.Chainable
import CFDI.Types.StationType
import CFDI.XmlNode
import Data.Maybe (catMaybes)

data WaybillComplementLocation = WaybillComplementLocation
  { locStationType :: Maybe StationType
  , locDistanceTraveled :: Maybe Rational
  } deriving (Eq, Show)

instance Chainable WaybillComplementLocation where
  chain _ = ""

instance XmlNode WaybillComplementLocation where
  attributes loc = catMaybes
    [ attr "TipoEstacion" <$> locStationType loc
    , attr "DistanciaRecorrida" <$> locDistanceTraveled loc
    ]

  children n = []

  nodeName = const "Ubicacion"

  parseNode n = WaybillComplementLocation
    <$> parseAttribute "TipoEstacion" n
    <*> parseAttribute "DistanciaRecorrida" n
