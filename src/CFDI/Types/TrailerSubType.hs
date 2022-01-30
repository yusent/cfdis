module CFDI.Types.TrailerSubType where

import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Text.Read          (readMaybe)

newtype TrailerSubType = TrailerSubType Int deriving (Eq, Read, Show)

instance Type TrailerSubType where
  parseExpr ('C' : 'T' : 'R' : c@[_, _, _]) = justErr NotInCatalog maybeSubType
    where
      maybeSubType = TrailerSubType <$> (readMaybe c >>= isValid)
      isValid x
        | x > 0 && x < 33 = Just x
        | otherwise = Nothing
  parseExpr _ = Left NotInCatalog

  render (TrailerSubType x) = "CTR" ++ replicate (3 - length xStr) '0' ++ xStr
    where
      xStr = show x
