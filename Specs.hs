import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "a dummy type" $ do
        it "should be having a dummy value to test for" $ do
            4 + 1 `shouldBe` 5
            
