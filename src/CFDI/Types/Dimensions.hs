module CFDI.Types.Dimensions where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Dimensions = Dimensions Text deriving (Eq, Show)

instance Chainable Dimensions where
  chain (Dimensions d) = d

instance Type Dimensions where
  parseExpr str
    | matchTest regExp str = Right . Dimensions $ pack str
    | otherwise = Left
                $ DoesNotMatchExpr "[0-9]{2}[/]{1}[0-9]{2}[/]{1}[0-9]{2}cm\
                                   \|[0-9]{2}[/]{1}[0-9]{2}[/]{1}[0-9]{2}plg"
    where
      regExp = mkRegex "^[0-9]{2}/[0-9]{2}/[0-9]{2}cm\
                       \|[0-9]{2}/[0-9]{2}/[0-9]{2}plg$"

  render (Dimensions d) = unpack d
