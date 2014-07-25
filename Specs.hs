import Test.Hspec
import PokerHand
import Data.List (nub)

main :: IO ()
main = hspec $ do
    describe "Card type" $ do
        it "should be extracted from a String" $ do
            card "As" `shouldBe` Card Ace Spade
    
        it "should have suit of four different possible suits" $ do
            let myCards = cards "Ah As Ad Ac" 
                suits = map suit myCards
            length (nub suits) `shouldBe` 4

        it "should have 13 distinct values" $ do
            let myCards = cards "2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks As"
                values = map value myCards
            length (nub values) `shouldBe` 13

        it "should have strictly ordered values" $ do
            let myCards = cards "2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks As"
                values = map value myCards
                isOrdered [x] = True
                isOrdered (x:y:xs) = x < y && isOrdered (y:xs)
            isOrdered values `shouldBe` True

    describe "Hand type" $ do
        it "should be comparable" $ do
            let hand1 = HighCard [Ten, Nine, Eight, Six, Five]
                hand2 = HighCard [Ten, Eight, Six, Five, Four]
            compare hand1 hand2 `shouldBe` GT

        it "should be computed as best ranking hand from a list of 7 Cards" $ do
            let myCards = cards "5s 2h Th 6d 9c 8d 3h" 
            hand myCards `shouldBe` HighCard [Ten, Nine, Eight, Six, Five]

        it "should have a null value for players who fold" $ do
            let myCards = cards "5s 2h Th 6d 9c 8d"
            hand myCards `shouldBe` Fold
            
    describe "Score type" $ do
        it "should be computed from a list of lists of hands" $ do
            let player1 = cards "5s 2h Th 6d 9c 8d 3h" 
                player2 = cards "5s 2h Th 6d 9c 8d"
            scores [player1,player2] `shouldBe` [(HighCard [Ten, Nine, Eight, Six, Five], True)
                                                ,(Fold, False)]

        it "should contains no winner when everyone folds" $ do
            let player1 = cards "2h Th 6d 9c 8d 3h" 
                player2 = cards "5s 2h Th 6d 9c 8d"
            scores [player1,player2] `shouldBe` [(Fold, False)
                                                ,(Fold, False)]
            
    describe "process function" $ do
        it "should take lines of cards and output scores" $ do
            let input = "5s 2h Th 6d 9c 8d 3h\n5s 2h Th 6d 9c 8d\n"
                output = "5s 2h Th 6d 9c 8d 3h High Card (winner)\n5s 2h Th 6d 9c 8d\n"
            process input `shouldBe` output
            

                   

            
            
     
