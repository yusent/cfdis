module CFDI.Types.GlobalInvoiceYear where

import CFDI.Chainable
import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Data.Text (pack)
import Text.Read (readMaybe)

newtype GlobalInvoiceYear = GlobalInvoiceYear Int deriving (Eq, Read, Show)

instance Chainable GlobalInvoiceYear where
  chain (GlobalInvoiceYear m) = pack $ show m

instance Type GlobalInvoiceYear where
  parseExpr c = justErr NotInCatalog maybeMonths
    where
      maybeMonths = GlobalInvoiceYear <$> (readMaybe c >>= isValid)
      isValid x
        | x > 2021 = Just x
        | otherwise = Nothing

  render (GlobalInvoiceYear x) = show x
