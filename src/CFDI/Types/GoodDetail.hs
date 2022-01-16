module CFDI.Types.GoodDetail where

import CFDI.Chainable
import CFDI.Types.KGWeight
import CFDI.Types.PositiveInt
import CFDI.Types.WeightUnit
import CFDI.XmlNode

data GoodDetail = GoodDetail
  { goodDetailWeightUnit :: WeightUnit
  , goodDetailGrossWeight :: KGWeight
  , goodDetailNetWeight :: KGWeight
  , goodDetailTareWeight :: KGWeight
  , goodDetailPiecesQty :: Maybe PositiveInt
  } deriving (Eq, Show)

instance Chainable GoodDetail where
  chain _ = ""

instance XmlNode GoodDetail where
  attributes n =
    [ attr "UnidadPeso" $ goodDetailWeightUnit n
    , attr "PesoBruto" $ goodDetailGrossWeight n
    , attr "PesoNeto" $ goodDetailNetWeight n
    , attr "PesoTara" $ goodDetailTareWeight n
    ]

  optionalAttributes n =
    [ attr "NumPiezas" <$> goodDetailPiecesQty n
    ]

  nodeName = const "DetalleMercancia"

  parseNode n = GoodDetail
    <$> requireAttribute "UnidadPeso" n
    <*> requireAttribute "PesoBruto" n
    <*> requireAttribute "PesoNeto" n
    <*> requireAttribute "PesoTara" n
    <*> parseAttribute "NumPiezas" n
