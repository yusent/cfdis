module CFDI.Types.LocationOrigin where

import CFDI.Chainable
import CFDI.Types.Country
import CFDI.Types.HarborType
import CFDI.Types.LocationOriginID
import CFDI.Types.Name
import CFDI.Types.RFC
import CFDI.Types.Station
import CFDI.Types.StationName
import CFDI.Types.TaxId
import CFDI.XmlNode
import Data.Maybe (catMaybes)
import Data.Time.LocalTime (LocalTime)

data LocationOrigin = LocationOrigin
  { locOrigID :: Maybe LocationOriginID
  , locOrigIssuerRFC :: Maybe RFC
  , locOrigIssuerName :: Maybe Name
  , locOrigIssuerTaxID :: Maybe TaxId
  , locOrigIssuerAddress :: Maybe Country
  , locOrigDepartureStation :: Maybe Station
  , locOrigDepartureStationName :: Maybe StationName
  , locOrigSeaTraffic :: Maybe HarborType
  , locOrigDepartureTime :: LocalTime
  } deriving (Eq, Show)

instance Chainable LocationOrigin where
  chain _ = ""

instance XmlNode LocationOrigin where
  attributes n = catMaybes
    [ attr "IDOrigen" <$> locOrigID n
    , attr "RFCRemitente" <$> locOrigIssuerRFC n
    , attr "NombreRemitente" <$> locOrigIssuerName n
    , attr "NumRegIdTrib" <$> locOrigIssuerTaxID n
    , attr "ResidenciaFiscal" <$> locOrigIssuerAddress n
    , attr "NumEstacion" <$> locOrigDepartureStation n
    , attr "NombreEstacion" <$> locOrigDepartureStationName n
    , attr "NavegacionTrafico" <$> locOrigSeaTraffic n
    , Just . attr "FechaHoraSalida" $ locOrigDepartureTime n
    ]

  nodeName = const "Origen"

  parseNode n = LocationOrigin
    <$> parseAttribute "IDOrigen" n
    <*> parseAttribute "RFCRemitente" n
    <*> parseAttribute "NombreRemitente" n
    <*> parseAttribute "NumRegIdTrib" n
    <*> parseAttribute "ResidenciaFiscal" n
    <*> parseAttribute "NumEstacion" n
    <*> parseAttribute "NombreEstacion" n
    <*> parseAttribute "NavegacionTrafico" n
    <*> requireAttribute "FechaHoraSalida" n
