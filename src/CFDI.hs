{-# LANGUAGE OverloadedStrings #-}

module CFDI where

import CFDI.Chain           (chain)
import CFDI.CSD             (signWithCSD)
import CFDI.Parser
import CFDI.Renderer        (render)
import CFDI.Types           (CFDI, signature)
import Control.Error.Safe   (justErr)
import Data.Text            (Text, append, pack)
import Text.XML.Light       (parseXMLDoc, ppTopElement)
import Text.XML.Light.Lexer (XmlSource)

originalChain :: CFDI -> Text
originalChain cfdi = "||" `append` chain cfdi `append` "||"

parse :: XmlSource s => s -> Parsed CFDI
parse xmlSource = justErr MalformedXML (parseXMLDoc xmlSource) >>= parseCFDI

signCFDIWith :: FilePath -> CFDI -> IO (Either Text CFDI)
signCFDIWith csdPemPath cfdi =
  fmap (fmap addSignatureToCFDI) . signWithCSD csdPemPath $ originalChain cfdi
  where
    addSignatureToCFDI sig = cfdi { signature = sig }

toXML :: CFDI -> Text
toXML = pack . ppTopElement . render
