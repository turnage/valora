import Img
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Img.Pixel" $ do
      describe "RGBA Pixel" $ do
        it "can apply" $ do
          apply (RGBA (0, 0, 0, 1)) (RGBA (1, 1, 1, 0.5)) `shouldBe` (RGBA (0.5, 0.5, 0.5, 1))
        it "can raster" $ do raster (RGBA (1, 0, 0, 1)) `shouldBe` (255, 0, 0)
