module CFDI.Types.MovedGood where

import CFDI.Chainable
import CFDI.Types.LocationDestinationID
import CFDI.Types.LocationOriginID
import CFDI.Types.InOutWay
import CFDI.Types.Quantity
import CFDI.XmlNode

data MovedGood = MovedGood
  { movedGoodQuantity :: Quantity
  , movedGoodOrigin :: LocationOriginID
  , movedGoodDestination :: LocationDestinationID
  , movedGoodInOutWay :: Maybe InOutWay
  } deriving (Eq, Show)

instance Chainable MovedGood where
  chain _ = ""

instance XmlNode MovedGood where
  attributes n =
    [ attr "Cantidad" $ movedGoodQuantity n
    , attr "IDOrigen" $ movedGoodOrigin n
    , attr "IDDestino" $ movedGoodDestination n
    ]

  optionalAttributes n =
    [ attr "CvesTransporte" <$> movedGoodInOutWay n
    ]

  nodeName = const "CantidadTransporta"

  parseNode n = MovedGood
    <$> requireAttribute "Cantidad" n
    <*> requireAttribute "IDOrigen" n
    <*> requireAttribute "IDDestino" n
    <*> parseAttribute "CvesTransporte" n
