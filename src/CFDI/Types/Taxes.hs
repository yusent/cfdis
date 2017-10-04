module CFDI.Types.Taxes where

import CFDI.Types.Amount
import CFDI.Types.RetainedTaxes
import CFDI.Types.TransferedTaxes
import CFDI.XmlNode
import Data.Maybe                 (catMaybes)

data Taxes = Taxes
  { retainedTaxes   :: Maybe RetainedTaxes
  , totalTransfered :: Maybe Amount
  , totalRetained   :: Maybe Amount
  , transferedTaxes :: Maybe TransferedTaxes
  } deriving (Eq, Show)

instance XmlNode Taxes where
  children n = catMaybes
    [ renderNode <$> retainedTaxes n
    , renderNode <$> transferedTaxes n
    ]

  nodeName = const "Impuestos"

  optionalAttributes n =
    [ attr "TotalImpuestosRetenidos"   <$> totalRetained n
    , attr "TotalImpuestosTrasladados" <$> totalTransfered n
    ]

  parseNode n = Taxes
    <$> parseChild "Retenciones" n
    <*> parseAttribute "TotalImpuestosTrasladados" n
    <*> parseAttribute "TotalImpuestosRetenidos" n
    <*> parseChild "Traslados" n
