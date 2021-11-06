module CFDI.Types.Station where

import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Data.Text (Text, pack, unpack)

newtype Station = Station Text deriving (Eq, Show)

instance Type Station where
  parseExpr = justErr NotInCatalog . fmap Station . maybeValidCode
    where
      maybeValidCode c = if isValid c then Just (pack c) else Nothing
      isValid ('P' : 'M' : c@[_, _, _]) = let n = read c in n > 0 && n < 121
      isValid ('E' : 'A' : c@[_, _, _]) = let n = read c in n > 0 && n < 562
      isValid ('E' : 'F' : c@[_, _, _]) = let n = read c in n > 0
      isValid ('E' : 'F' : c@[_, _, _, _]) = let n = read c in n > 999 && n < 2608

  render (Station x) = unpack x
