module CFDI.Types.VehicleModelYear where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text (pack)

newtype VehicleModelYear = VehicleModelYear Int deriving (Eq, Show)

instance Chainable VehicleModelYear where
  chain (VehicleModelYear y) = pack $ show y

instance Type VehicleModelYear where
  parseExpr str
    | year >= 1900 && year < 2100 = Right $ VehicleModelYear year
    | otherwise = Left $ DoesNotMatchExpr "(19[0-9]{2}|20[0-9]{2})"
    where year = read str

  render (VehicleModelYear y) = render y
