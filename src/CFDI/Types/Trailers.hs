module CFDI.Types.Trailers where

import CFDI.Chainable
import CFDI.Types.Trailer
import CFDI.XmlNode
import Data.Text (intercalate)

data Trailers = Trailers
  { trTrailers :: [Trailer]
  } deriving (Eq, Show)

instance Chainable Trailers where
  chain = intercalate "|" . map chain . trTrailers

instance XmlNode Trailers where
  children n = renderNode <$> trTrailers n

  nodeName = const "Remolques"

  parseNode n = do
    locs <- parseChildren "Remolque" n
    case locs of
      (_ : _ : _ : _) -> Left  $ ExpectedNoMoreThan 2 "Remolque"
      trailers  -> Right $ Trailers trailers
