import qualified Data.Vector as V
import Test.Hspec

import Shape

main :: IO ()
main =
  hspec $ do
    describe "Polgyons" $ do
      describe "Rectangle" $ do
        it "Yield correct edges" $ do
          edges (Rect (Point (0, 0), Point (10, 10))) `shouldBe`
            V.fromList
              [ Edge (Point (0, 10), Point (0, 0))
              , Edge (Point (0, 0), Point (10, 0))
              , Edge (Point (10, 0), Point (10, 10))
              , Edge (Point (10, 10), Point (0, 10))
              ]
