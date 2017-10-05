module CFDI.Types.RetainedTaxes where

import CFDI.Chainable
import CFDI.Types.RetainedTax
import CFDI.XmlNode
import Data.Text              (intercalate)

newtype RetainedTaxes = RetainedTaxes [RetainedTax] deriving (Eq, Show)

instance Chainable RetainedTaxes where
  chain (RetainedTaxes ts) = intercalate "|" $ chain <$> ts

instance XmlNode RetainedTaxes where
  children (RetainedTaxes ts) = renderNode <$> ts

  nodeName = const "Retenciones"

  parseNode n = do
    retTaxes <- parseChildren "Retencion" n
    case retTaxes of
      [] -> Left  $ ExpectedAtLeastOne "Retencion"
      _  -> Right $ RetainedTaxes retTaxes
