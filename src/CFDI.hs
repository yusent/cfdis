{-# LANGUAGE OverloadedStrings #-}

module CFDI where

import CFDI.Chain           (chain)
import CFDI.CSD             (signWithCSD)
import CFDI.PAC             (PAC, StampError, getPacStamp)
import CFDI.Parser          (ParseError(MalformedXML), Parsed, parseCFDI)
import CFDI.Renderer        (render)
import CFDI.Types
  ( CFDI
  , Complement(..)
  , complement
  , pacStamp
  , signature
  )
import Control.Error.Safe   (justErr)
import Data.Maybe           (fromMaybe)
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

stamp :: PAC p => CFDI -> p -> IO (Either StampError CFDI)
stamp cfdi =
  fmap (fmap addStampToCFDI) . getPacStamp (toXML cfdi)
  where
    addStampToCFDI stamp' =
      cfdi { complement = Just (complement' { pacStamp = Just stamp' }) }
    complement' = fromMaybe (Complement Nothing) $ complement cfdi

toXML :: CFDI -> Text
toXML = pack . ppTopElement . render
