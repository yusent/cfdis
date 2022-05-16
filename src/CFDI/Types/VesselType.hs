module CFDI.Types.VesselType where

import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Text.Read          (readMaybe)

newtype VesselType = VesselType Int deriving (Eq, Read, Show)

instance Type VesselType where
  parseExpr ('B' : c@[_, _]) = justErr NotInCatalog maybeSubType
    where
      maybeSubType = VesselType <$> (readMaybe c >>= isValid)
      isValid x
        | x > 0 && x < 16 = Just x
        | otherwise = Nothing
  parseExpr _ = Left NotInCatalog

  render (VesselType x) = "B" ++ replicate (2 - length xStr) '0' ++ xStr
    where
      xStr = show x
