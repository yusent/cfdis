{-# LANGUAGE OverloadedStrings #-}

module CFDI.PAC where

import CFDI.Types (PacStamp)
import Data.Text  (Text)

type XML = Text

class PAC p where
  getPacStamp :: XML -> p -> IO (Either StampError PacStamp)

data StampError = StampError
  { stampErrMsg  :: Text
  , stampErrCode :: Maybe Text
  }
