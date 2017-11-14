module CFDI.Types.RelatedDocumentId where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype RelatedDocumentId = RelatedDocumentId Text deriving (Eq, Show)

instance Chainable RelatedDocumentId where
  chain (RelatedDocumentId u) = u

instance Type RelatedDocumentId where
  parseExpr str
    | matchTest regExp str = Right . RelatedDocumentId $ pack str
    | otherwise = Left
                $ DoesNotMatchExpr
                    "([a-f0-9A-F]{8}-[a-f0-9A-F]{4}-[a-f0-9A-F]{4}-[a-f0-9A-F]\
                    \{4}-[a-f0-9A-F]{12})|([0-9]{3}-[0-9]{2}-[0-9]{9})"
    where
      regExp = mkRegex "^([a-f0-9A-F]{8}-[a-f0-9A-F]{4}-[a-f0-9A-F]{4}-\
                       \[a-f0-9A-F]{4}-[a-f0-9A-F]{12})|([0-9]{3}-[0-9]\
                       \{2}-[0-9]{9})$"

  render (RelatedDocumentId u) = unpack u
