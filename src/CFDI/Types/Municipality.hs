module CFDI.Types.Municipality where

import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Text.Read          (readMaybe)

data Municipality = Municipality Int deriving (Eq, Read, Show)

instance Type Municipality where
  parseExpr c = justErr NotInCatalog maybeMunicipality
    where
      maybeMunicipality = readMaybe c >>= isValid >>= return . Municipality
      isValid x
        | x > 0 && x < 571 = Just x
        | otherwise = Nothing

  render (Municipality x) = replicate (3 - length xStr) '0' ++ xStr
    where
      xStr = show x
