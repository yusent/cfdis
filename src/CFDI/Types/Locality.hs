module CFDI.Types.Locality where

import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Text.Read          (readMaybe)

newtype Locality = Locality Int deriving (Eq, Read, Show)

instance Type Locality where
  parseExpr c = justErr NotInCatalog maybeLocality
    where
      maybeLocality = Locality <$> (readMaybe c >>= isValid)
      isValid x
        | x >  0 && x < 63 = Just x
        | x > 65 && x < 70 = Just x
        | otherwise = Nothing

  render (Locality x) = replicate (2 - length xStr) '0' ++ xStr
    where
      xStr = show x
