module CFDI.Types.UUID where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype UUID = UUID Text deriving (Eq, Show)

instance Type UUID where
  parseExpr str
    | matchTest regExp str = Right . UUID $ pack str
    | otherwise = Left
                $ DoesNotMatchExpr "[a-f0-9A-F]{8}-[a-f0-9A-F]{4}-[a-f0-9A-F]\
                                   \{4}-[a-f0-9A-F]{4}-[a-f0-9A-F]{12}"
    where
      regExp = mkRegex "^[a-f0-9A-F]{8}-[a-f0-9A-F]{4}-[a-f0-9A-F]{4}-\
                       \[a-f0-9A-F]{4}-[a-f0-9A-F]{12}$"

  render (UUID u) = unpack u
