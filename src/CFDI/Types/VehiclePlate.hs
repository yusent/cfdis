module CFDI.Types.VehiclePlate where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype VehiclePlate = VehiclePlate Text deriving (Eq, Show)

instance Chainable VehiclePlate where
  chain (VehiclePlate p) = p

instance Type VehiclePlate where
  parseExpr str
    | matchTest regExp str = Right . VehiclePlate $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^(?!.*\\s)-]{6,7}"
    where
      regExp = mkRegex "^[^(?!.*\\s)-]{6,7}$"

  render (VehiclePlate p) = unpack p
