module CFDI.Types.Catalog where

import Data.Text (Text)

class Catalog c where
  fromCode :: Text -> Maybe c

  toCode :: c -> Text
