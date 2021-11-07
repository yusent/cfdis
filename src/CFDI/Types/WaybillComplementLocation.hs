module CFDI.Types.WaybillComplementLocation where

import CFDI.Chainable
import CFDI.Types.LocationAddress
import CFDI.Types.LocationDestination
import CFDI.Types.LocationOrigin
import CFDI.Types.StationType
import CFDI.XmlNode
import Data.Maybe (catMaybes)

data WaybillComplementLocation = WaybillComplementLocation
  { locStationType :: Maybe StationType
  , locDistanceTraveled :: Maybe Rational
  , locOrigin :: Maybe LocationOrigin
  , locDestination :: Maybe LocationDestination
  , locAddress :: Maybe LocationAddress
  } deriving (Eq, Show)

instance Chainable WaybillComplementLocation where
  chain _ = ""

instance XmlNode WaybillComplementLocation where
  attributes loc = catMaybes
    [ attr "TipoEstacion" <$> locStationType loc
    , attr "DistanciaRecorrida" <$> locDistanceTraveled loc
    ]

  children n = catMaybes
    [ renderNode <$> locOrigin n
    , renderNode <$> locDestination n
    , renderNode <$> locAddress n
    ]

  nodeName = const "Ubicacion"

  parseNode n = WaybillComplementLocation
    <$> parseAttribute "TipoEstacion" n
    <*> parseAttribute "DistanciaRecorrida" n
    <*> parseChild "Origen" n
    <*> parseChild "Destino" n
    <*> parseChild "Domicilio" n
