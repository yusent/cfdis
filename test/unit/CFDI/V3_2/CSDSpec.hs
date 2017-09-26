{-# LANGUAGE OverloadedStrings #-}

module CFDI.V3_2.CSDSpec
  ( spec
  ) where

import CFDI.V3_2.CSD
import Prelude      hiding (readFile)
import Data.Text           (Text, unpack)
import Data.Text.IO        (readFile)
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import System.Directory    (removeFile)
import System.IO.Temp      (writeSystemTempFile)
import Test.Hspec

spec :: Spec
spec = do
  describe "CFDI.V3_2.CSD.csdKeyToPem" $ do
    it "transforms CSD key from DER to PEM format" $ do
      eitherErrOrPem <-
        csdKeyToPem "test/csd/CSD01_AAA010101AAA.key" "12345678a"
      eitherErrOrPem `shouldBe` Right testPEM

  describe "CFDI.V3_2.CSD.getCsdCerData" $ do
    it "gets CSD certificate data" $ do
      eitherErrOrCsdCerData <- getCsdCerData "test/csd/CSD01_AAA010101AAA.cer"
      eitherErrOrCsdCerData `shouldBe` Right CsdCerData
        { cerExpiresAt = LocalTime (ModifiedJulianDay 59352) (TimeOfDay 3 54 56)
        , cerNumber    = "30001000000300023708"
        , cerToText    = testCerText
        }

  describe "CFDI.V3_2.CSD.signWithCSD" $ do
    it "signs some text with a CSD PEM" $ do
      pemFilePath <- writeSystemTempFile "csd.pem" (unpack testPEM)
      eitherErrOrText <- signWithCSD pemFilePath "TEST"
      eitherErrOrText `shouldBe` Right testSignedText
      removeFile pemFilePath

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

testPEM :: Text
testPEM =
  "-----BEGIN PRIVATE KEY-----\n\
  \MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCXVHLByBCIMIr7\n\
  \wGp7Rp2FSDt/u8k3Q9bZCqKWy/rSko0RaNRKz3RicT94MU+TvgAiA0Fzfh0vbjFg\n\
  \2J2lEWpUiBfQjhQnYfzh9j5/lJpDeHLpa9g2pys7VSDvInVzOnPWh5+YTqO4Q8Vz\n\
  \At7jrhldxUuDm5MU1fvTbLzgFdpTHtu0h60gdA4bi7q1G2cF5vv6+Hbc929ri4AX\n\
  \Pg4sveTxvjCLu6DSjPPWgTRnI0ycPvZPlsyYj3d2aUICHnWAQ/vEFsZWAoMYZ7Wt\n\
  \5PcpYFymFwmJNJNKmrrwSdY0kVX+QY4yFGzrLWkKefZ5IB4eOXPYJ8CUVetfDVtH\n\
  \zHg87ikdAgMBAAECggEALS8Z1KJXzVIxLVoWcRh0kAcxPMJlIgsvaz6xrTTaf2Ui\n\
  \mcAjIvMuXPZTbR/MEuD4SS+Pq1xMeoz8UV5cM50vkm3QLoU9n0SyrQVJQ+6q4Npl\n\
  \9SwuMqNXVS/l1YEEcJNTYwq7rE5OtAYIPn7s7i5dhJIUKgeZsu7xcf9VpdLgjVCD\n\
  \qGgJw/EfhagR7iPF+PKoeyRyBZI9xuHmtElHVgn2/Qv/16UJv0YpAqRgVq7YQzZC\n\
  \c7yo0Y2+3dqHabRg+MnIKkN4pBFBzYxsjwM7YUDk/8zFlF5kwCS74ep0JWWSYAJ1\n\
  \3DYDtCYSyWk1DvxX9Srv/S2htZM6MnhboafjLch4QQKBgQDRGGLpYdqGt6/cXKQe\n\
  \JGWFrG33AMiYKrd4NOw7LK7kzrQESeaeAXSwr2eOnNV3tDMyslkjpC05m3Lbefsh\n\
  \Ul6Qj/Qj9PEIpv7e4X4r++O/FsA9X6iQFicMEDzRYYjm4AfFggYrhzmjXh2rNACL\n\
  \KRX5i9wIRGQuoAG7KuZyYWSBuwKBgQC5Rsv75S6FNUpKe8RC2nw13Vaf9uua2W1+\n\
  \spg2pWfKqw88vvFATQOj9A9aFJ+wqrvwRziua5xtbch9gHK7M9Nnl565Tk8muueO\n\
  \OUBaFeHYXsDaYZfTFILOZU4/b6//r6QK2cO892VXyUydbRXavCpRX8s2EoxtwfFG\n\
  \mgbStX+HBwKBgQCICHKJXXU7QhPyrH7FcW5vKgAcu3DFtrzIQr4RvX9HMsdhJucX\n\
  \kuDk9ijMWnJyv1Szvd5KVsxpdx2hdlmQkzMcn9r47alGtMaKIG/ik6zWrCmDhFF4\n\
  \9ECRE5tNqUPU2JmVwILdHMu94kQxFtLntmIqiPgslLoMr2KQ71cfwQcPcwKBgQCk\n\
  \iNKtqCFf+qs26iKonA6iZyV+eXFR2rT6RvAV114NBUxKzebBC6On/h2ECbymz3iH\n\
  \MTiM7NPF+jCKA3/f725WGLfEKF7yLhlknEMhvT0LQVpSlUiXEyf20tBiVXUew4QS\n\
  \fsDtF2bQRtvbEfzOezu5eDCmnGJJNmpmIHLevH+8EQKBgF9Ff09RISQJHbABka8f\n\
  \wj8sdBKWG3TUQ2SwQ9U3L/Y/unuyaRUF+J3wFRYBMQGu0jzLG5TFfAVZAc3VJCBj\n\
  \xG6K8WnJS6OM9ycV0qBa2WnkC7M7uAt4K9IEIqlOljY/R2tBN7qHZwE7nCLS88rv\n\
  \L5YWIiKp71SlXyoGLfM0h7bl\n\
  \-----END PRIVATE KEY-----\n"

testSignedText :: Text
testSignedText =
  "kopDKx5K63tyd/ZEQVWR9AJs9wi7qfK6veN3IYCRjeXHusfyMDKQizvUmQhsuuj3QkY/NvU3PpqP\
  \apJSNsbHQJxsTNTtsvxaCZxjV0HkuUp39ppHYeKM36Ed76UtN/0hMqOqYL29CIS8QOD+NCSjSDxm\
  \lk5CcJNgW9sQC/Kfe/w1E//tx9nWYenBVAI8gPfH3eA/wFm2glZAR0RHmWKQizq+HlugV7HeZnLk\
  \xnlMewgp8Ayr7D4D0skrbhwhPkkFwXoEHmux5/oUAHCX2LlTh0eLt9fCWjFJ2yOxY+S7bt9apIyA\
  \AO0nvzz1n7b042174OUICjtibQsoVkQLK8zjPw=="
