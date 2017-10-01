module CFDI.Types.Customs where

import CFDI.Types.Catalog
import Data.Set     (fromList, member)
import Data.Text    (pack, unpack)
import Text.Read    (readMaybe)

data Custom = Custom Int deriving (Eq, Show)

instance Catalog Custom where
  fromCode c = readMaybe (unpack c) >>= isValid >>= return . Custom
    where
      isValid x
        | x `member` validCodes = Just x
        | otherwise = Nothing
      validCodes = fromList
        [ 1, 2, 5, 6, 7, 8, 11, 12, 14, 16, 17, 18, 19, 20, 22, 23, 24, 25, 26
        , 27, 28, 30, 31, 33, 34, 37, 38, 39, 40, 42, 43, 44, 46, 47, 48, 50
        , 51, 52, 53, 64, 65, 67, 73, 75, 80, 81, 82, 83, 84
        ]

  toCode (Custom x) = pack $ replicate (2 - length xStr) '0' ++ xStr
    where
      xStr = show x
