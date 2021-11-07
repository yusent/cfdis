module CFDI.Types.LocationAddress where

import CFDI.Chainable
import CFDI.Types.AddressNumber
import CFDI.Types.AddressReference
import CFDI.Types.AddressState
import CFDI.Types.Country
import CFDI.Types.Description100
import CFDI.Types.Description120
import CFDI.Types.InternationalZipCode
import CFDI.XmlNode
import Data.Maybe (catMaybes)
import Data.Time.LocalTime (LocalTime)

data LocationAddress = LocationAddress
  { locAddrStreet :: Description100
  , locAddrNumber :: Maybe AddressNumber
  , locAddrAptNumber :: Maybe AddressNumber
  , locAddrSuburb :: Maybe Description120
  , locAddrCity :: Maybe Description120
  , locAddrReference :: Maybe AddressReference
  , locAddrMunicipality :: Maybe Description120
  , locAddrState :: AddressState
  , locAddrCountry :: Country
  , locAddrZipCode :: InternationalZipCode
  } deriving (Eq, Show)

instance Chainable LocationAddress where
  chain _ = ""

instance XmlNode LocationAddress where
  attributes n = catMaybes
    [ Just  . attr "Calle" $ locAddrStreet n
    , attr "NumeroExterior" <$> locAddrNumber n
    , attr "NumeroInterior" <$> locAddrAptNumber n
    , attr "Colonia" <$> locAddrSuburb n
    , attr "Localidad" <$> locAddrCity n
    , attr "Referencia" <$> locAddrReference n
    , attr "Municipio" <$> locAddrMunicipality n
    , Just  . attr "Estado" $ locAddrState n
    , Just  . attr "Pais" $ locAddrCountry n
    , Just  . attr "CodigoPostal" $ locAddrZipCode n
    ]

  nodeName = const "Domicilio"

  parseNode n = LocationAddress
    <$> requireAttribute "Calle" n
    <*> parseAttribute "NumeroExterior" n
    <*> parseAttribute "NumeroInterior" n
    <*> parseAttribute "Colonia" n
    <*> parseAttribute "Localidad" n
    <*> parseAttribute "Referencia" n
    <*> parseAttribute "Municipio" n
    <*> requireAttribute "Estado" n
    <*> requireAttribute "Pais" n
    <*> requireAttribute "CodigoPostal" n
