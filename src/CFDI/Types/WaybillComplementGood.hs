module CFDI.Types.WaybillComplementGood where

import CFDI.Chainable
import CFDI.XmlNode
import CFDI.Types.Dimensions
import CFDI.Types.HazardousMaterialID
import CFDI.Types.MeasurementUnit
import CFDI.Types.ProductDescription
import CFDI.Types.ProductOrService
import CFDI.Types.ProductUnit
import CFDI.Types.Quantity
import CFDI.Types.STCCID
import CFDI.Types.YesNo
import Data.Maybe (catMaybes)

data WaybillComplementGood = WaybillComplementGood
  { wcGoodCode :: Maybe ProductOrService
  , wcGoodSTCCCode :: Maybe STCCID
  , wcGoodDescription :: Maybe ProductDescription
  , wcGoodQuantity :: Maybe Quantity
  , wcGoodMeasurementUnit :: Maybe MeasurementUnit
  , wcGoodProductUnit :: Maybe ProductUnit
  , wcGoodDimensions :: Maybe Dimensions
  , wcGoodHazardous :: Maybe YesNo
  , wcGoodHazardousID :: Maybe HazardousMaterialID
  } deriving (Eq, Show)

instance Chainable WaybillComplementGood where
  chain _ = ""

instance XmlNode WaybillComplementGood where
  attributes n = catMaybes
    [ attr "BienesTransp" <$> wcGoodCode n
    , attr "ClaveSTCC" <$> wcGoodSTCCCode n
    , attr "Descripcion" <$> wcGoodDescription n
    , attr "Cantidad" <$> wcGoodQuantity n
    , attr "ClaveUnidad" <$> wcGoodMeasurementUnit n
    , attr "Unidad" <$> wcGoodProductUnit n
    , attr "Dimensiones" <$> wcGoodDimensions n
    , attr "MaterialPeligroso" <$> wcGoodHazardous n
    , attr "CveMaterialPeligroso" <$> wcGoodHazardousID n
    ]

  children n = catMaybes
    [
    ]

  nodeName = const "Mercancia"

  parseNode n = WaybillComplementGood
    <$> parseAttribute "BienesTransp" n
    <*> parseAttribute "ClaveSTCC" n
    <*> parseAttribute "Descripcion" n
    <*> parseAttribute "Cantidad" n
    <*> parseAttribute "ClaveUnidad" n
    <*> parseAttribute "Unidad" n
    <*> parseAttribute "Dimensiones" n
    <*> parseAttribute "MaterialPeligroso" n
    <*> parseAttribute "CveMaterialPeligroso" n
