module CFDISpec (spec) where

import CFDI
import CFDI.Types
import Data.Maybe          (fromJust, isJust)
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Test.Hspec
import Text.XML.Light
  ( Element(..)
  , QName(..)
  , onlyElems
  , parseXMLDoc
  )

cfdi :: CFDI
cfdi = CFDI
  (Just (CertificateNumber "00001000001212121212"))
  (Just "MIIGejCCBGKgAwIBAgIUMDAwMDEwMDAwMDA0MDM1MDE3NDIwDQYJKoZIhvcNAQELBQAwgg\
        \GyMTgwNgYDVQQDDC9BLkMuIGRlbCBTZXJ2aWNpbyBkZSBBZG1pbmlzdHJhY2nDs24gVHJp\
        \YnV0YXJpYTEvMC0GA1UECgwmU2VydmljaW8gZGUgQWRtaW5pc3RyYWNpw7NuIFRyaWJ1dG\
        \FyaWExODA2BgNVBAsML0FkbWluaXN0cmFjacOzbiBkZSBTZWd1cmlkYWQgZGUgbGEgSW5m\
        \b3JtYWNpw7NuMR8wHQYJKoZIhvcNAQkBFhBhY29kc0BzYXQuZ29iLm14MSYwJAYDVQQJDB\
        \1Bdi4gSGlkYWxnbyA3NywgQ29sLiBHdWVycmVybzEOMAwGA1UEEQwFMDYzMDAxCzAJBgNV\
        \BAYTAk1YMRkwFwYDVQQIDBBEaXN0cml0byBGZWRlcmFsMRQwEgYDVQQHDAtDdWF1aHTDqW\
        \1vYzEVMBMGA1UELRMMU0FUOTcwNzAxTk4zMV0wWwYJKoZIhvcNAQkCDE5SZXNwb25zYWJs\
        \ZTogQWRtaW5pc3RyYWNpw7NuIENlbnRyYWwgZGUgU2VydmljaW9zIFRyaWJ1dGFyaW9zIG\
        \FsIENvbnRyaWJ1eWVudGUwHhcNMTYwODI2MTgzODMwWhcNMjAwODI2MTgzOTEwWjCB6DEk\
        \MCIGA1UEAxMbSk9TRSBKT0FRVUlOIExJTkNPTE4gQ09SUkFMMSQwIgYDVQQpExtKT1NFIE\
        \pPQVFVSU4gTElOQ09MTiBDT1JSQUwxJDAiBgNVBAoTG0pPU0UgSk9BUVVJTiBMSU5DT0xO\
        \IENPUlJBTDELMAkGA1UEBhMCTVgxMjAwBgkqhkiG9w0BCQEWI21hcmNvbWFydGluZXouY2\
        \9uc3VsdG9yZXNAZ21haWwuY29tMRYwFAYDVQQtEw1MSUNKNzkwNDA4SDM4MRswGQYDVQQF\
        \ExJMSUNKNzkwNDA4SEJDTlJRMDUwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQ\
        \C8+x6GljZ0U4olZYclYQXh3cyFuw1p79zo6js/9xrjW7kc3VHJAv8smcnSkikki7P+J3wl\
        \z6NoqGIYXvHurXqszLNucScGyjgB81G2hEDv4o8ybid36C89vckRjExz8aM39uJe1VQJuH\
        \Rlwcuzmp8tvDX37nkTfgFGV5x+Gh+69QsS3gncI+x8utNPbdO/456svs8sTa2h/ZmWT5Kk\
        \W2LmRHxHDsKf5xB2A+Le4MYIAMMUOYLENbGffih4OaFnQlkUJ6YK1vmmhEalYw1IKY8HxY\
        \zbePZx6AisT8JYWsVCc0wGnio4gTLbKFMQKGrDLF3wx4llCmS98TmJHFYqpmixAgMBAAGj\
        \TzBNMAwGA1UdEwEB/wQCMAAwCwYDVR0PBAQDAgPYMBEGCWCGSAGG+EIBAQQEAwIFoDAdBg\
        \NVHSUEFjAUBggrBgEFBQcDBAYIKwYBBQUHAwIwDQYJKoZIhvcNAQELBQADggIBAJbhVgVp\
        \iLaRoZJOfv0hY4SPJSVlD+nhIMN61TYAU8egA8rnAnBkfjG71urkI03B/FvcUoCwMnViDX\
        \z5tyUcSa1a6URSTpV9loHQ/QLWeTE4bWf5L8HkbjYfpyNMYAzcSc+FXcDhcsiljrJohzZm\
        \NBG106MDOJup9BreUa/OD4Az4Yfvj0HoufGGEi7CbP00+L1hp4/DuJ2m8+FjaGlWI0Vkc0\
        \X3CwtjUl+M/BrD3IweeVSThIoATdltdJVj2aBHyGSSi211BVxJTrpCdse30nCIcjiEuuCI\
        \2bBAIuX/RkF8QyZxQvA619IiA4ix9LfR3q87QNXuILLd4lkZG1wdYbrUcGwgx5anBbDb/H\
        \d4To+bmHGAMrKmeEsSly0yTqpE+BvajvMPCXYe18c5uYSjP/0dg4UHD5khmMr/aI6bcX4m\
        \CceaIxoLBWdW9YDHY7FKad1Byvlg5YO3j8PxhOKtHjXkf2tO6XDr5tb4xkmYK8unEqgMWq\
        \efvLVNAI4iNMP8cX1fGpd8SN1Lr4TaoiON0mZps8rcUS2DRP2+l9VdtnBV8BO0DXCFXFo9\
        \4FTlC9sWLcSsoO8+ZQjzKIRe2XyzfoXXj4Ax2ZNoQdItK7GOS10AD/KsoHvlj0O4leN1pG\
        \Pg5SYmI1ppvP5IIQKqqL6rLdW1iCenyhLhWdgMZq82t2Xf")
  Income
  [ Complement
      (Just (PacStamp
              (Just (SatLegend "AVISO PARA EL CONTRIBUYENTE"))
              (CompanyRfc "SAT970701NN3")
              (CertificateNumber "00001000000403258748")
              "VxwwW+2KLIFNppKkfSUMdMTxP/hOzxcLjG1TGSjMrYEZ3goBp18d/LO37jfE7blA\
              \xJH0nnWWYQEH4JPCpyvXSjA1HRooYlsvV81dSDQF9W0ojPpqgKt5E46+dwNNL3yn\
              \wkcQIg0GhLMzZgEhH3Yw/PGHm0dby02Z6gzm6GyFyA50d1fpco/+fQpiBdlxneH+\
              \n+3uO8OnN8V4shM/vFKIbW6HkhjDgOeJLb2mRM7rSAT9jdbFDkfQri/df3laVUs/\
              \gpEoyXEpK7E5C53yfgqWyTASSEPFlgoSSlEVRL7jC7Pb6eY2EynFgQXt6uemzqQV\
              \oCHha+aDHxiaOlrUI+Xu1g=="
              "AXvgW3lSloJK7iSUJsHiBmLAcdCnQZi0yA8XwODvucH6kgdJTTHr256z9XBjIRFG\
              \Q+eUlmSs4USAS3rz2+DMfLS27EkEeReEFF/Mhcf0bM7U91Gm9p7OvAFs9WPbFzhx\
              \P+gvnzr4KdJ9O4CAsfyVNtlTJ+Z+cquDZw5Yun25xfOqObFtv/m7g2+c9dWbBAgf\
              \2vEGUemy1hg9i8nEBCFkx6TIH3SyojLsXwnIoNWQ3CIZzpydfBnVawddcCZcoUiA\
              \NUQBM5rfgYf1CzUoopEIo1l6+UdbyefKNpvYpejcrBUSPY6Q8uQFvf+rm6jtTrsI\
              \c2LNlhiV5fL9mb8MGMWiKw=="
              (LocalTime
                (ModifiedJulianDay 57953)
                (TimeOfDay 14 31 15))
              (UUID "BA87BCD8-411B-41CB-BA9B-81B1A8327099")
              (PacStampVersion 1.1)))
  ]
  (Concepts
    [ Concept
        (Amount "1090.52")
        [CustomInfo (ImportNumber "12  12  1212  1212121")]
        (ProductDescription "COMIDA MEXICANA")
        (Just (Amount "0"))
        MU_ACT
        (Just (ProductId "PROD12"))
        (ProductOrService 91111700)
        (Quantity "1")
        (Just (ConceptTaxes
                (Just (ConceptRetainedTaxes
                        [ ConceptRetainedTax
                            (Amount "0")
                            (TaxBase "0.000001")
                            Rate
                            (TaxRate "0.100000")
                            ISR
                        ]))
                (Just (ConceptTransferedTaxes
                        [ ConceptTransferedTax
                            (Amount "174.48")
                            (TaxBase "1090.52")
                            Rate
                            (TaxRate "0.160000")
                            IVA
                        ]))))
        (Just (ProductUnit "N/A"))
        (Amount "1090.52")
    ])
  (Just (Confirmation "AbcD5"))
  CUR_MXN
  (Just (Amount "0"))
  (Just (ExchangeRate "12.121212"))
  (Just (Folio "12"))
  (LocalTime
    (ModifiedJulianDay 57953)
    (TimeOfDay 14 27 3))
  (ZipCode 22115)
  (Issuer
    (Just (Name "EMISOR DE PRUEBA"))
    (RFC "XAXX010101000")
    (PeopleWithBusinessActivities))
  (Just (PaymentConditions "CONDICIONES DE PAGO DE PRUEBA"))
  (Just OneTimePayment)
  (Recipient
    (Just GeneralExpenses)
    (Just (Name "RECEPTOR DE PRUEBA"))
    (RFC "XEXX010101000")
    (Just (TaxId "1234567890"))
    (Just CTY_MEX))
  (Just (RelatedCfdis
          [RelatedCfdi (UUID "12121212-1212-1212-1212-121212121212")]
          Substitution))
  (Just (Series "ABC"))
  (Just "AXvgW3lSloJK7iSUJsHiBmLAcdCnQZi0yA8XwODvucH6kgdJTTHr256z9XBjIRFGQ+eUlm\
        \Ss4USAS3rz2+DMfLS27EkEeReEFF/Mhcf0bM7U91Gm9p7OvAFs9WPbFzhxP+gvnzr4KdJ9\
        \O4CAsfyVNtlTJ+Z+cquDZw5Yun25xfOqObFtv/m7g2+c9dWbBAgf2vEGUemy1hg9i8nEBC\
        \Fkx6TIH3SyojLsXwnIoNWQ3CIZzpydfBnVawddcCZcoUiANUQBM5rfgYf1CzUoopEIo1l6\
        \+UdbyefKNpvYpejcrBUSPY6Q8uQFvf+rm6jtTrsIc2LNlhiV5fL9mb8MGMWiKw==")
  (Amount "1090.52")
  (Just (Taxes 
          (Just (RetainedTaxes
                  [ RetainedTax
                      (Amount "0")
                      ISR
                  ]))
          (Just (Amount "174.48"))
          (Just (Amount "0"))
          (Just (TransferedTaxes
                  [ TransferedTax 
                      (Amount "174.48")
                      Rate
                      (TaxRate "0.160000")
                      IVA
                  ]))))
  (Amount "1265.00")
  (Version 3.3)
  (Just Cash)

spec :: Spec
spec = do
  describe "CFDI.parseCfdiFile" $ do
    eitherErrOrCfdi <- runIO $ parseCfdiFile "test/xml/invoice_3_3.xml"

    it "parses invoices from an xml file path" $ do
      eitherErrOrCfdi `shouldBe` Right cfdi

  describe "CFDI.parseCfdiXml" $ do
    xmlSource <- runIO $ readFile "test/xml/invoice_3_3.xml"

    it "parses invoices from an xml source" $ do
      parseCfdiXml xmlSource `shouldBe` Right cfdi

  describe "CFDI.toXML" $ do
    it "returns a parsable XML" $ do
      parseCfdiXml (toXML cfdi) `shouldBe` Right cfdi

    it "returns an XML with its elements in the right order" $ do
      let maybeXML = parseXMLDoc $ toXML cfdi
      maybeXML `shouldSatisfy` isJust

      let xml = fromJust maybeXML
      elName xml `shouldBe`
        QName "Comprobante" (Just "http://www.sat.gob.mx/cfd/3") (Just "cfdi")

      let cfdiElems = onlyElems $ elContent xml
          elemNames = map (qName . elName) cfdiElems
      elemNames `shouldBe`
        [ "CfdiRelacionados"
        , "Emisor"
        , "Receptor"
        , "Conceptos"
        , "Impuestos"
        , "Complemento"
        ]

      let conElems = onlyElems . elContent . head . onlyElems . elContent
                   $ cfdiElems !! 3
          conElemNames = map (qName . elName) conElems
      conElemNames `shouldBe` ["Impuestos", "InformacionAduanera"]

      let conTaxElemNames = map (qName . elName) . onlyElems . elContent
                          $ head conElems
      conTaxElemNames `shouldBe` ["Traslados", "Retenciones"]

      let taxElemNames = map (qName . elName) . onlyElems . elContent
                       $ cfdiElems !! 4
      taxElemNames `shouldBe` ["Retenciones", "Traslados"]
