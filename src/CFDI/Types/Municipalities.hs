module CFDI.Types.Municipalities where

import CFDI.Types.Catalog
import Data.Text    (pack, unpack)
import Text.Read    (readMaybe)

data Municipality = Municipality Int deriving (Eq, Read, Show)

instance Catalog Municipality where
  fromCode c = readMaybe (unpack c) >>= isValid >>= return . Municipality
    where
      isValid x
        | x > 0 && x < 571 = Just x
        | otherwise = Nothing

  toCode (Municipality x) = pack $ replicate (3 - length xStr) '0' ++ xStr
    where
      xStr = show x
