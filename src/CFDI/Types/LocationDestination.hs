module CFDI.Types.LocationDestination where

import CFDI.Chainable
import CFDI.Types.Country
import CFDI.Types.HarborType
import CFDI.Types.LocationDestinationID
import CFDI.Types.Name
import CFDI.Types.RFC
import CFDI.Types.Station
import CFDI.Types.StationName
import CFDI.Types.TaxId
import CFDI.XmlNode
import Data.Maybe (catMaybes)
import Data.Time.LocalTime (LocalTime)

data LocationDestination = LocationDestination
  { locDestID :: Maybe LocationDestinationID
  , locDestRecipientRFC :: Maybe RFC
  , locDestRecipientName :: Maybe Name
  , locDestRecipientTaxID :: Maybe TaxId
  , locDestRecipientAddress :: Maybe Country
  , locDestArrivalStation :: Maybe Station
  , locDestArrivalStationName :: Maybe StationName
  , locDestSeaTraffic :: Maybe HarborType
  , locDestArrivalTime :: LocalTime
  } deriving (Eq, Show)

instance Chainable LocationDestination where
  chain _ = ""

instance XmlNode LocationDestination where
  attributes n = catMaybes
    [ attr "IDDestino" <$> locDestID n
    , attr "RFCDestinatario" <$> locDestRecipientRFC n
    , attr "NombreDestinatario" <$> locDestRecipientName n
    , attr "NumRegIdTrib" <$> locDestRecipientTaxID n
    , attr "ResidenciaFiscal" <$> locDestRecipientAddress n
    , attr "NumEstacion" <$> locDestArrivalStation n
    , attr "NombreEstacion" <$> locDestArrivalStationName n
    , attr "NavegacionTrafico" <$> locDestSeaTraffic n
    , Just . attr "FechaHoraProgLlegada" $ locDestArrivalTime n
    ]

  nodeName = const "Destino"

  parseNode n = LocationDestination
    <$> parseAttribute "IDDestino" n
    <*> parseAttribute "RFCDestinatario" n
    <*> parseAttribute "NombreDestinatario" n
    <*> parseAttribute "NumRegIdTrib" n
    <*> parseAttribute "ResidenciaFiscal" n
    <*> parseAttribute "NumEstacion" n
    <*> parseAttribute "NombreEstacion" n
    <*> parseAttribute "NavegacionTrafico" n
    <*> requireAttribute "FechaHoraProgLlegada" n
