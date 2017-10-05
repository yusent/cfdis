module CFDI.Types.Suburb where

import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Text.Read          (readMaybe)

newtype Suburb = Suburb Int deriving (Eq, Read, Show)

instance Type Suburb where
  parseExpr c = justErr NotInCatalog maybeSuburb
    where
      maybeSuburb = readMaybe c >>= isValid >>= return . Suburb
      isValid x
        | x > 0 && x < 10000 = Just x
        | otherwise = Nothing

  render (Suburb x) = replicate (4 - length xStr) '0' ++ xStr
    where
      xStr = show x
