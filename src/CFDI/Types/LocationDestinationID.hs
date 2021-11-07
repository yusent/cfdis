module CFDI.Types.LocationDestinationID where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype LocationDestinationID = LocationDestinationID Text deriving (Eq, Show)

instance Chainable LocationDestinationID where
  chain (LocationDestinationID i) = i

instance Type LocationDestinationID where
  parseExpr str
    | matchTest regExp str = Right . LocationDestinationID $ pack str
    | otherwise = Left $ DoesNotMatchExpr "DE[0-9]{6}"
    where
      regExp = mkRegex "^DE[0-9]{6}$"

  render (LocationDestinationID i) = unpack i
