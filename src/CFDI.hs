{-# LANGUAGE OverloadedStrings #-}

module CFDI where

import CFDI.Chain     (chain)
import CFDI.CSD       (signWithCSD)
import CFDI.Renderer  (render)
import CFDI.Types     (CFDI, signature)
import Data.Text      (Text, append, pack)
import Text.XML.Light (ppTopElement)

originalChain :: CFDI -> Text
originalChain cfdi = "||" `append` chain cfdi `append` "||"

signCFDIWith :: FilePath -> CFDI -> IO (Either Text CFDI)
signCFDIWith csdPemPath cfdi = do
  eitherErrOrSignature <- signWithCSD csdPemPath $ originalChain cfdi
  return $ case eitherErrOrSignature of
    Right sig -> Right $ cfdi { signature = sig }
    Left  err -> Left err

toXML :: CFDI -> Text
toXML = pack . ppTopElement . render
