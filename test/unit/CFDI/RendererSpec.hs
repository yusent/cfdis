module CFDI.RendererSpec (spec) where

import CFDI.Parser
import CFDI.Renderer
import Test.Hspec

spec :: Spec
spec = do
  describe "CFDI.Renderer.toXML" $ do
    xmlSource <- runIO $ readFile "test/xml/invoice.xml"

    it "renders a complete representation of a CFDI as XML" $ do
      let Right parsedCFDI = parseCFDI xmlSource
          renderedCFDI     = toXML parsedCFDI

      parseCFDI renderedCFDI `shouldBe` Right parsedCFDI
