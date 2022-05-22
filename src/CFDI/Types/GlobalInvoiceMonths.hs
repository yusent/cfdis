module CFDI.Types.GlobalInvoiceMonths where

import CFDI.Chainable
import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Data.Text (pack)
import Text.Read (readMaybe)

newtype GlobalInvoiceMonths = GlobalInvoiceMonths Int deriving (Eq, Read, Show)

instance Chainable GlobalInvoiceMonths where
  chain (GlobalInvoiceMonths m) = pack $ show m

instance Type GlobalInvoiceMonths where
  parseExpr c = justErr NotInCatalog maybeMonths
    where
      maybeMonths = GlobalInvoiceMonths <$> (readMaybe c >>= isValid)
      isValid x
        | x > 0 && x < 19 = Just x
        | otherwise = Nothing

  render (GlobalInvoiceMonths x) = replicate (2 - length xStr) '0' ++ xStr
    where
      xStr = show x
