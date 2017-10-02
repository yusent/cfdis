module CFDI.Types.Custom where

import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Data.Set           (fromList, member)
import Text.Read          (readMaybe)

data Custom = Custom Int deriving (Eq, Show)

instance Type Custom where
  parseExpr c = justErr NotInCatalog maybeCustom
    where
      maybeCustom = readMaybe c >>= isValid >>= return . Custom
      isValid x
        | x `member` validCodes = Just x
        | otherwise = Nothing
      validCodes = fromList
        [ 1, 2, 5, 6, 7, 8, 11, 12, 14, 16, 17, 18, 19, 20, 22, 23, 24, 25, 26
        , 27, 28, 30, 31, 33, 34, 37, 38, 39, 40, 42, 43, 44, 46, 47, 48, 50
        , 51, 52, 53, 64, 65, 67, 73, 75, 80, 81, 82, 83, 84
        ]

  render (Custom x) = replicate (2 - length xStr) '0' ++ xStr
    where
      xStr = show x
