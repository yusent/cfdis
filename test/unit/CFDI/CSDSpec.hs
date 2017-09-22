{-# LANGUAGE OverloadedStrings #-}

module CFDI.CSDSpec (spec) where

import CFDI.CSD
import Data.Either         (isRight)
import Prelude      hiding (readFile)
import Data.Text           (Text)
import Data.Text.IO        (readFile)
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "CFDI.CSD.csdKeyToPem" $ do
    it "transforms CSD key from DER to PEM format" $ do
      eitherErrOrPem <-
        csdKeyToPem "test/csd/CSD01_AAA010101AAA.key" "12345678a"
      testPEM <- readFile "test/csd/CSD01_AAA010101AAA.pem"
      eitherErrOrPem `shouldBe` Right testPEM

  describe "CFDI.CSD.getCsdCerData" $ do
    eitherErrOrCsdCerData <-
      runIO $ getCsdCerData "test/csd/CSD01_AAA010101AAA.cer"

    it "gets CSD certificate data" $ do
      eitherErrOrCsdCerData `shouldBe` Right CsdCerData
        { cerExpiresAt = LocalTime (ModifiedJulianDay 59352) (TimeOfDay 3 54 56)
        , cerNumber    = "30001000000300023708"
        , cerToText    = testCerText
        }

  describe "CFDI.CSD.signWithCSD" $ do
    it "signs some text with a CSD PEM" $ do
      eitherErrOrText <- signWithCSD "test/csd/CSD01_AAA010101AAA.pem" "TEST"
      eitherErrOrText `shouldBe` Right testSignedText

testCerText :: Text
testCerText =
  "MIIF+TCCA+GgAwIBAgIUMzAwMDEwMDAwMDAzMDAwMjM3MDgwDQYJKoZIhvcNAQELBQAwggFmMSAw\
  \HgYDVQQDDBdBLkMuIDIgZGUgcHJ1ZWJhcyg0MDk2KTEvMC0GA1UECgwmU2VydmljaW8gZGUgQWRt\
  \aW5pc3RyYWNpw7NuIFRyaWJ1dGFyaWExODA2BgNVBAsML0FkbWluaXN0cmFjacOzbiBkZSBTZWd1\
  \cmlkYWQgZGUgbGEgSW5mb3JtYWNpw7NuMSkwJwYJKoZIhvcNAQkBFhphc2lzbmV0QHBydWViYXMu\
  \c2F0LmdvYi5teDEmMCQGA1UECQwdQXYuIEhpZGFsZ28gNzcsIENvbC4gR3VlcnJlcm8xDjAMBgNV\
  \BBEMBTA2MzAwMQswCQYDVQQGEwJNWDEZMBcGA1UECAwQRGlzdHJpdG8gRmVkZXJhbDESMBAGA1UE\
  \BwwJQ295b2Fjw6FuMRUwEwYDVQQtEwxTQVQ5NzA3MDFOTjMxITAfBgkqhkiG9w0BCQIMElJlc3Bv\
  \bnNhYmxlOiBBQ0RNQTAeFw0xNzA1MTgwMzU0NTZaFw0yMTA1MTgwMzU0NTZaMIHlMSkwJwYDVQQD\
  \EyBBQ0NFTSBTRVJWSUNJT1MgRU1QUkVTQVJJQUxFUyBTQzEpMCcGA1UEKRMgQUNDRU0gU0VSVklD\
  \SU9TIEVNUFJFU0FSSUFMRVMgU0MxKTAnBgNVBAoTIEFDQ0VNIFNFUlZJQ0lPUyBFTVBSRVNBUklB\
  \TEVTIFNDMSUwIwYDVQQtExxBQUEwMTAxMDFBQUEgLyBIRUdUNzYxMDAzNFMyMR4wHAYDVQQFExUg\
  \LyBIRUdUNzYxMDAzTURGUk5OMDkxGzAZBgNVBAsUEkNTRDAxX0FBQTAxMDEwMUFBQTCCASIwDQYJ\
  \KoZIhvcNAQEBBQADggEPADCCAQoCggEBAJdUcsHIEIgwivvAantGnYVIO3+7yTdD1tkKopbL+tKS\
  \jRFo1ErPdGJxP3gxT5O+ACIDQXN+HS9uMWDYnaURalSIF9COFCdh/OH2Pn+UmkN4culr2DanKztV\
  \IO8idXM6c9aHn5hOo7hDxXMC3uOuGV3FS4ObkxTV+9NsvOAV2lMe27SHrSB0DhuLurUbZwXm+/r4\
  \dtz3b2uLgBc+Diy95PG+MIu7oNKM89aBNGcjTJw+9k+WzJiPd3ZpQgIedYBD+8QWxlYCgxhnta3k\
  \9ylgXKYXCYk0k0qauvBJ1jSRVf5BjjIUbOstaQp59nkgHh45c9gnwJRV618NW0fMeDzuKR0CAwEA\
  \AaMdMBswDAYDVR0TAQH/BAIwADALBgNVHQ8EBAMCBsAwDQYJKoZIhvcNAQELBQADggIBABKj0DCN\
  \L1lh44y+OcWFrT2icnKF7WySOVihx0oR+HPrWKBMXxo9KtrodnB1tgIx8f+Xjqyphhbw+juDSeDr\
  \b99PhC4+E6JeXOkdQcJt50Kyodl9URpCVWNWjUb3F/ypa8oTcff/eMftQZT7MQ1Lqht+xm3QhVox\
  \TIASce0jjsnBTGD2JQ4uT3oCem8bmoMXV/fk9aJ3v0+ZIL42MpY4POGUa/iTaawklKRAL1Xj9IdI\
  \R06RK68RS6xrGk6jwbDTEKxJpmZ3SPLtlsmPUTO1kraTPIo9FCmU/zZkWGpd8ZEAAFw+ZfI+bdXB\
  \fvdDwaM2iMGTQZTTEgU5KKTIvkAnHo9O45SqSJwqV9NLfPAxCo5eRR2OGibd9jhHe81zUsp5GdE1\
  \mZiSqJU82H3cu6BiE+D3YbZeZnjrNSxBgKTIf8w+KNYPM4aWnuUMl0mLgtOxTUXi9MKnUccq3GZL\
  \A7bx7Zn211yPRqEjSAqybUMVIOho6aqzkfc3WLZ6LnGU+hyHuZUfPwbnClb7oFFz1PlvGOpNDsUb\
  \0qP42QCGBiTUseGugAzqOP6EYpVPC73gFourmdBQgfayaEvi3xjNanFkPlW1XEYNrYJB4yNjphFr\
  \vWwTY86vL2o8gZN0Utmc5fnoBTfM9r2zVKmEi6FUeJ1iaDaVNv47te9iS1ai4V4vBY8r"

testSignedText :: Text
testSignedText =
  "kopDKx5K63tyd/ZEQVWR9AJs9wi7qfK6veN3IYCRjeXHusfyMDKQizvUmQhsuuj3QkY/NvU3PpqP\
  \apJSNsbHQJxsTNTtsvxaCZxjV0HkuUp39ppHYeKM36Ed76UtN/0hMqOqYL29CIS8QOD+NCSjSDxm\
  \lk5CcJNgW9sQC/Kfe/w1E//tx9nWYenBVAI8gPfH3eA/wFm2glZAR0RHmWKQizq+HlugV7HeZnLk\
  \xnlMewgp8Ayr7D4D0skrbhwhPkkFwXoEHmux5/oUAHCX2LlTh0eLt9fCWjFJ2yOxY+S7bt9apIyA\
  \AO0nvzz1n7b042174OUICjtibQsoVkQLK8zjPw=="
