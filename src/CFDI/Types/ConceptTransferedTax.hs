module CFDI.Types.ConceptTransferedTax where

import CFDI.Chainable
import CFDI.Types.Amount
import CFDI.Types.FactorType
import CFDI.Types.Tax
import CFDI.Types.TaxRate
import CFDI.XmlNode
import Data.Maybe            (catMaybes)

data ConceptTransferedTax = ConceptTransferedTax
  { conTransAmount     :: Maybe Amount
  , conTransBase       :: Amount
  , conTransFactorType :: FactorType
  , conTransRate       :: Maybe TaxRate
  , conTransTax        :: Tax
  } deriving (Eq, Show)

instance Chainable ConceptTransferedTax where
  chain c = conTransBase
        <@> conTransTax
        <~> conTransFactorType
        <~> conTransRate
        <~> conTransAmount
        <~> (c, "")

instance XmlNode ConceptTransferedTax where
  attributes n =
    [ attr "Base"       $ conTransBase n
    , attr "TipoFactor" $ conTransFactorType n
    , attr "Impuesto"   $ conTransTax n
    ] ++ catMaybes
    [ attr "Importe"    <$> conTransAmount n
    , attr "TasaOCuota" <$> conTransRate n
    ]

  nodeName = const "Traslado"

  parseNode n = ConceptTransferedTax
    <$> parseAttribute "Importe" n
    <*> requireAttribute "Base" n
    <*> requireAttribute "TipoFactor" n
    <*> parseAttribute "TasaOCuota" n
    <*> requireAttribute "Impuesto" n
