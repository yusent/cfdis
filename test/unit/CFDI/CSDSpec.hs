{-# LANGUAGE OverloadedStrings #-}

module CFDI.CSDSpec (spec) where

import CFDI.CSD
import Data.Either         (isRight)
import Prelude      hiding (readFile)
import Data.Text           (Text)
import Data.Text.IO        (readFile)
import Test.Hspec

spec :: Spec
spec = do
  describe "CFDI.CSD.csdKeyToPem" $ do
    it "transforms CSD key from DER to PEM format" $ do
      eitherErrOrPem <-
        csdKeyToPem "test/csd/CSD01_AAA010101AAA.key" "12345678a"
      eitherErrOrPem `shouldSatisfy` isRight
      let Right pem = eitherErrOrPem
      testPEM <- readFile "test/csd/CSD01_AAA010101AAA.pem"
      pem `shouldBe` testPEM

  describe "CFDI.CSD.signWithCSD" $ do
    it "signs some text with a CSD PEM" $ do
      eitherErrOrText <- signWithCSD "test/csd/CSD01_AAA010101AAA.pem" "TEST"
      eitherErrOrText `shouldSatisfy` isRight
      let Right text = eitherErrOrText
      text `shouldBe` testSignedText

testSignedText :: Text
testSignedText =
  "kopDKx5K63tyd/ZEQVWR9AJs9wi7qfK6veN3IYCRjeXHusfyMDKQizvUmQhsuuj3QkY/NvU3PpqP\
  \apJSNsbHQJxsTNTtsvxaCZxjV0HkuUp39ppHYeKM36Ed76UtN/0hMqOqYL29CIS8QOD+NCSjSDxm\
  \lk5CcJNgW9sQC/Kfe/w1E//tx9nWYenBVAI8gPfH3eA/wFm2glZAR0RHmWKQizq+HlugV7HeZnLk\
  \xnlMewgp8Ayr7D4D0skrbhwhPkkFwXoEHmux5/oUAHCX2LlTh0eLt9fCWjFJ2yOxY+S7bt9apIyA\
  \AO0nvzz1n7b042174OUICjtibQsoVkQLK8zjPw=="
