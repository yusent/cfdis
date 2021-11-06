module CFDI.Types.LocationOrigin where

import CFDI.Chainable
import CFDI.Types.LocationOriginID
import CFDI.XmlNode
import Data.Maybe (catMaybes)

data LocationOrigin = LocationOrigin
  { locOrigID :: Maybe LocationOriginID
  } deriving (Eq, Show)

instance Chainable LocationOrigin where
  chain _ = ""

instance XmlNode LocationOrigin where
  attributes n = catMaybes
    [ attr "IDOrigen" <$> locOrigID n
    ]

  children n =
    [
    ]

  nodeName = const "Origen"

  parseNode n = LocationOrigin
    <$> parseAttribute "IDOrigen" n
