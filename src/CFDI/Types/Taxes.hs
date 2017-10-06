module CFDI.Types.Taxes where

import CFDI.Chainable
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

instance Chainable Taxes where
  chain c = retainedTaxes
        <@> totalRetained
        <~> transferedTaxes
        <~> totalTransfered
        <~> (c, "")

instance XmlNode Taxes where
  attributes n = catMaybes
    [ attr "TotalImpuestosRetenidos"   <$> totalRetained n
    , attr "TotalImpuestosTrasladados" <$> totalTransfered n
    ]

  children n = catMaybes
    [ renderNode <$> retainedTaxes n
    , renderNode <$> transferedTaxes n
    ]

  nodeName = const "Impuestos"

  parseNode n = Taxes
    <$> parseChild "Retenciones" n
    <*> parseAttribute "TotalImpuestosTrasladados" n
    <*> parseAttribute "TotalImpuestosRetenidos" n
    <*> parseChild "Traslados" n
