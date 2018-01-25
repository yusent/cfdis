module CFDI.Types.RFC where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Char        (isDigit, isUpper)
import Data.Text        (Text, pack, unpack)

newtype RFC = RFC Text deriving (Eq, Show)

instance Chainable RFC where
  chain (RFC r) = r

instance Type RFC where
  parseExpr str
    | isValidRfc str = Right . RFC $ pack str
    | otherwise = Left
                $ DoesNotMatchExpr "[A-Z&Ñ]{3,4}[0-9]{2}(0[1-9]|1[012])(0[1-9]|\
                                   \[12][0-9]|3[01])[A-Z0-9]{2}[0-9A]"

  render (RFC r) = unpack r

isValidRfc :: String -> Bool
isValidRfc = isValidReverseRfc . reverse
  where
    isValidReverseRfc (h2 : h1 : h0 : d1 : d0 : m1 : m0 : y1 : y0 : r) =
         (length r == 3 || length r == 4)
      && all (\c -> isUpper c || c == '&') r
      && (isDigit y0 && isDigit y1)
      && (m0 == '0' && isDigit m1 || m0 == '1' && (m1 `elem` ("012" :: String)))
      && (    d0 == '0' && isDigit d1
           || (d0 `elem` ("12" :: String)) && isDigit d1
           || d0 == '3' && (d1 `elem` ("01" :: String))
         )
      && (isUpper h0 || isDigit h0)
      && (isUpper h1 || isDigit h1)
      && (h2 == 'A' || isDigit h2)
    isValidReverseRfc _ = False
