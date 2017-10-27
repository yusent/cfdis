module CFDI.CSDSpec
  ( spec
  ) where

import CFDI.CSD
import Prelude      hiding (readFile)
import Data.Either         (isRight)
import Data.Text           (Text, pack, unpack)
import Data.Text.IO        (readFile)
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import System.Directory    (removeFile)
import System.IO.Temp      (writeSystemTempFile)
import Test.Hspec

spec :: Spec
spec = do
  describe "CFDI.CSD.csdKeyToPem" $ do
    it "transforms CSD key from DER to PEM format" $ do
      eitherErrOrPem <-
        csdKeyToPem "test/csd/CSD01_AAA010101AAA.key" "12345678a"
      eitherErrOrPem `shouldBe` Right testPEM

  describe "CFDI.CSD.exportCsdAsPfx" $ do
    it "exports a CSD to a PFX format" $ do
      pemFilePath <- writeSystemTempFile "csd.pem" (unpack testPEM)
      eitherErrOrPfx <- exportCsdAsPfx pemFilePath testCerPEM "somepass"
      eitherErrOrPfx `shouldSatisfy` isRight

  describe "CFDI.CSD.getCsdCerData" $ do
    it "gets CSD certificate data" $ do
      eitherErrOrCsdCerData <- getCsdCerData "test/csd/CSD01_AAA010101AAA.cer"
      eitherErrOrCsdCerData `shouldBe` Right CsdCerData
        { cerExpiresAt = LocalTime (ModifiedJulianDay 59352) (TimeOfDay 3 54 56)
        , cerNumber    = "30001000000300023708"
        , cerToText    = testCerText
        }

  describe "CFDI.CSD.signWithCSD" $ do
    it "signs some text with a CSD PEM" $ do
      pemFilePath <- writeSystemTempFile "csd.pem" (unpack testPEM)
      eitherErrOrText <- signWithCSD pemFilePath "TEST"
      eitherErrOrText `shouldBe` Right testSignedText
      removeFile pemFilePath

testCerPEM :: Text
testCerPEM =
  "-----BEGIN CERTIFICATE-----\n\
  \MIIF+TCCA+GgAwIBAgIUMzAwMDEwMDAwMDAzMDAwMjM3MDgwDQYJKoZIhvcNAQEL\n\
  \BQAwggFmMSAwHgYDVQQDDBdBLkMuIDIgZGUgcHJ1ZWJhcyg0MDk2KTEvMC0GA1UE\n\
  \CgwmU2VydmljaW8gZGUgQWRtaW5pc3RyYWNpw7NuIFRyaWJ1dGFyaWExODA2BgNV\n\
  \BAsML0FkbWluaXN0cmFjacOzbiBkZSBTZWd1cmlkYWQgZGUgbGEgSW5mb3JtYWNp\n\
  \w7NuMSkwJwYJKoZIhvcNAQkBFhphc2lzbmV0QHBydWViYXMuc2F0LmdvYi5teDEm\n\
  \MCQGA1UECQwdQXYuIEhpZGFsZ28gNzcsIENvbC4gR3VlcnJlcm8xDjAMBgNVBBEM\n\
  \BTA2MzAwMQswCQYDVQQGEwJNWDEZMBcGA1UECAwQRGlzdHJpdG8gRmVkZXJhbDES\n\
  \MBAGA1UEBwwJQ295b2Fjw6FuMRUwEwYDVQQtEwxTQVQ5NzA3MDFOTjMxITAfBgkq\n\
  \hkiG9w0BCQIMElJlc3BvbnNhYmxlOiBBQ0RNQTAeFw0xNzA1MTgwMzU0NTZaFw0y\n\
  \MTA1MTgwMzU0NTZaMIHlMSkwJwYDVQQDEyBBQ0NFTSBTRVJWSUNJT1MgRU1QUkVT\n\
  \QVJJQUxFUyBTQzEpMCcGA1UEKRMgQUNDRU0gU0VSVklDSU9TIEVNUFJFU0FSSUFM\n\
  \RVMgU0MxKTAnBgNVBAoTIEFDQ0VNIFNFUlZJQ0lPUyBFTVBSRVNBUklBTEVTIFND\n\
  \MSUwIwYDVQQtExxBQUEwMTAxMDFBQUEgLyBIRUdUNzYxMDAzNFMyMR4wHAYDVQQF\n\
  \ExUgLyBIRUdUNzYxMDAzTURGUk5OMDkxGzAZBgNVBAsUEkNTRDAxX0FBQTAxMDEw\n\
  \MUFBQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAJdUcsHIEIgwivvA\n\
  \antGnYVIO3+7yTdD1tkKopbL+tKSjRFo1ErPdGJxP3gxT5O+ACIDQXN+HS9uMWDY\n\
  \naURalSIF9COFCdh/OH2Pn+UmkN4culr2DanKztVIO8idXM6c9aHn5hOo7hDxXMC\n\
  \3uOuGV3FS4ObkxTV+9NsvOAV2lMe27SHrSB0DhuLurUbZwXm+/r4dtz3b2uLgBc+\n\
  \Diy95PG+MIu7oNKM89aBNGcjTJw+9k+WzJiPd3ZpQgIedYBD+8QWxlYCgxhnta3k\n\
  \9ylgXKYXCYk0k0qauvBJ1jSRVf5BjjIUbOstaQp59nkgHh45c9gnwJRV618NW0fM\n\
  \eDzuKR0CAwEAAaMdMBswDAYDVR0TAQH/BAIwADALBgNVHQ8EBAMCBsAwDQYJKoZI\n\
  \hvcNAQELBQADggIBABKj0DCNL1lh44y+OcWFrT2icnKF7WySOVihx0oR+HPrWKBM\n\
  \Xxo9KtrodnB1tgIx8f+Xjqyphhbw+juDSeDrb99PhC4+E6JeXOkdQcJt50Kyodl9\n\
  \URpCVWNWjUb3F/ypa8oTcff/eMftQZT7MQ1Lqht+xm3QhVoxTIASce0jjsnBTGD2\n\
  \JQ4uT3oCem8bmoMXV/fk9aJ3v0+ZIL42MpY4POGUa/iTaawklKRAL1Xj9IdIR06R\n\
  \K68RS6xrGk6jwbDTEKxJpmZ3SPLtlsmPUTO1kraTPIo9FCmU/zZkWGpd8ZEAAFw+\n\
  \ZfI+bdXBfvdDwaM2iMGTQZTTEgU5KKTIvkAnHo9O45SqSJwqV9NLfPAxCo5eRR2O\n\
  \Gibd9jhHe81zUsp5GdE1mZiSqJU82H3cu6BiE+D3YbZeZnjrNSxBgKTIf8w+KNYP\n\
  \M4aWnuUMl0mLgtOxTUXi9MKnUccq3GZLA7bx7Zn211yPRqEjSAqybUMVIOho6aqz\n\
  \kfc3WLZ6LnGU+hyHuZUfPwbnClb7oFFz1PlvGOpNDsUb0qP42QCGBiTUseGugAzq\n\
  \OP6EYpVPC73gFourmdBQgfayaEvi3xjNanFkPlW1XEYNrYJB4yNjphFrvWwTY86v\n\
  \L2o8gZN0Utmc5fnoBTfM9r2zVKmEi6FUeJ1iaDaVNv47te9iS1ai4V4vBY8r\n\
  \-----END CERTIFICATE-----\n"

testCerText :: Text
testCerText =
  pack . concat . init . tail . lines $ unpack testCerPEM

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
