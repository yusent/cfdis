module CFDI.Types.SCTPermissionType where

import CFDI.Chainable
import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Data.Text (pack)
import Text.Read (readMaybe)

data SCTPermissionType
  = TPAF01
  | TPAF02
  | TPAF03
  | TPAF04
  | TPAF05
  | TPAF06
  | TPAF07
  | TPAF08
  | TPAF09
  | TPAF10
  | TPAF11
  | TPAF12
  | TPAF13
  | TPAF14
  | TPAF15
  | TPAF16
  | TPAF17
  | TPAF18
  | TPAF19
  | TPAF20
  | TPTM01
  | TPTA01
  | TPTA02
  | TPTA03
  | TPTA04
  | TPXX00
  deriving (Eq, Read, Show)

instance Chainable SCTPermissionType where
  chain = pack . render

instance Type SCTPermissionType where
  parseExpr = justErr NotInCatalog . readMaybe

  render = show
