module CFDI.Types.ConceptRetainedTaxes where

import CFDI.Chainable
import CFDI.Types.ConceptRetainedTax
import CFDI.XmlNode
import Data.Text                     (intercalate)

newtype ConceptRetainedTaxes =
  ConceptRetainedTaxes [ConceptRetainedTax] deriving (Eq, Show)

instance Chainable ConceptRetainedTaxes where
  chain (ConceptRetainedTaxes ts) = intercalate "|" $ chain <$> ts

instance XmlNode ConceptRetainedTaxes where
  children (ConceptRetainedTaxes ts) = renderNode <$> ts

  nodeName = const "Retenciones"

  parseNode n = do
    retTaxes <- parseChildren "Retencion" n
    case retTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Retencion"
      _  -> Right $ ConceptRetainedTaxes retTaxes
