import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Tile
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "LambdaTrek.Simulation.Sector" $ do
    describe "get" $ do
      it "should get the tile at 0, 0" $ do
        get 0 0 emptySector `shouldBe` Just EmptySpace

      it "should get the tile at 14, 14" $ do
        get 14 14 emptySector `shouldBe` Just EmptySpace

      it "should get Nothing for something out of bounds" $ do
        get 200 200 emptySector `shouldBe` Nothing
