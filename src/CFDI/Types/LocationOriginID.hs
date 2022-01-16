module CFDI.Types.LocationOriginID where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype LocationOriginID = LocationOriginID Text deriving (Eq, Show)

instance Chainable LocationOriginID where
  chain (LocationOriginID i) = i

instance Type LocationOriginID where
  parseExpr str
    | matchTest regExp str = Right . LocationOriginID $ pack str
    | otherwise = Left $ DoesNotMatchExpr "OR[0-9]{6}"
    where
      regExp = mkRegex "^OR[0-9]{6}$"

  render (LocationOriginID i) = unpack i
