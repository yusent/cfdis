module CFDI.Types.ImportNumber where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

data ImportNumber = ImportNumber Text deriving (Eq, Show)

instance Type ImportNumber where
  -- Skip sanitization so we can have double spaces
  parse = parseExpr

  parseExpr str
    | matchTest regExp str = Right . ImportNumber $ pack str
    | otherwise = Left
                $ DoesNotMatchExpr "[0-9]{2}  [0-9]{2}  [0-9]{4}  [0-9]{7}"
    where
      regExp = mkRegex "^[0-9]{2}  [0-9]{2}  [0-9]{4}  [0-9]{7}$"

  render (ImportNumber n) = unpack n