module CFDI.Catalogs.Suburbs where

import CFDI.Catalog
import Data.Text    (pack, unpack)
import Text.Read    (readMaybe)

data Suburb = Suburb Int deriving (Eq, Read, Show)

instance Catalog Suburb where
  fromCode c = readMaybe (unpack c) >>= isValid >>= return . Suburb
    where
      isValid x
        | x > 0 && x < 10000 = Just x
        | otherwise = Nothing

  toCode (Suburb x) = pack $ replicate (4 - length xStr) '0' ++ xStr
    where
      xStr = show x
