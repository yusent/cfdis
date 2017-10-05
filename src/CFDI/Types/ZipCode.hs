module CFDI.Types.ZipCode where

import CFDI.Chainable
import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Text.Read          (readMaybe)

newtype ZipCode = ZipCode Int deriving (Eq, Show)

instance Chainable ZipCode where
  chain (ZipCode c) = chain c

instance Type ZipCode where
  parseExpr c = justErr NotInCatalog maybeZipCode
    where
      maybeZipCode = readMaybe c >>= isValid >>= return . ZipCode
      isValid x
        | x > 0     && x <  1000 = Nothing
        | x > 16999 && x < 20000 = Nothing
        | x == 20640             = Nothing
        | x > 28469 && x < 28500 = Nothing
        | x > 52799 && x < 52900 = Nothing
        | x > 54499 && x < 54539 = Nothing
        | x > 54539 && x < 54570 = Nothing
        | x > 68929 && x < 68940 = Nothing
        | x > 90750 && x < 90754 = Nothing
        | x > 90754 && x < 90760 = Nothing
        | x > 91310 && x < 91315 = Nothing
        | x > 93769 && x < 93780 = Nothing
        | x > 95049 && x < 95670 = Nothing
        | x > 99999              = Nothing
        | otherwise = Just x

  render (ZipCode x) = replicate (5 - length xStr) '0' ++ xStr
    where
      xStr = show x
