module CFDI.Types.Localities where

import CFDI.Types.Catalog
import Data.Text    (pack, unpack)
import Text.Read    (readMaybe)

data Locality = Locality Int deriving (Eq, Read, Show)

instance Catalog Locality where
  fromCode c = readMaybe (unpack c) >>= isValid >>= return . Locality
    where
      isValid x
        | x >  0 && x < 63 = Just x
        | x > 65 && x < 70 = Just x
        | otherwise = Nothing

  toCode (Locality x) = pack $ replicate (2 - length xStr) '0' ++ xStr
    where
      xStr = show x
