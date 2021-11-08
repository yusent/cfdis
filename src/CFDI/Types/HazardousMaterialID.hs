module CFDI.Types.HazardousMaterialID where

import CFDI.Types.Type
import Data.Text (Text, pack, unpack)

newtype HazardousMaterialID = HazardousMaterialID Text deriving (Eq, Show)

instance Type HazardousMaterialID where
  parseExpr c
    | isValid c = Right . HazardousMaterialID $ pack c
    | otherwise = Left NotInCatalog
    where
      isValid ('M' : c@[_, _, _, _]) = let n = read c in n > 0 && n < 2347
      isValid _ = False

  render (HazardousMaterialID x) = unpack x
