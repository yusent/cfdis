module CFDI.V3_2
  ( signCFDIWith
  , module CFDI.V3_2.Chain
  , module CFDI.V3_2.Parser
  , module CFDI.V3_2.Renderer
  , module CFDI.V3_2.Types
  ) where

import CFDI.CSD           (signWithCSD)
import CFDI.V3_2.Chain
import CFDI.V3_2.Parser
import CFDI.V3_2.Renderer
import CFDI.V3_2.Types
import Data.Text          (Text)

signCFDIWith :: FilePath -> CFDI -> IO (Either Text CFDI)
signCFDIWith csdPemPath cfdi =
  fmap (fmap addSignatureToCFDI) . signWithCSD csdPemPath $ originalChain cfdi
  where
    addSignatureToCFDI sig = cfdi { signature = sig }
