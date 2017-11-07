{-# OPTIONS_GHC -fno-warn-orphans #-}

module CFDI.Types.Addenda where

import CFDI.XmlNode
import Text.XML.Light (Element(..), elChildren)

data Addenda = Addenda [Element] deriving (Eq, Show)

instance Eq Element where
  e == e' = elName e == elName e'

instance XmlNode Addenda where
  children (Addenda as) = as

  nodeName = const "Addenda"

  parseNode = Right . Addenda . elChildren
