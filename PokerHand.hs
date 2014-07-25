module PokerHand
where
import Data.List (sort)

data Card = Card Value Suit
    deriving (Show, Eq)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

data Suit = Heart | Spade | Diamond | Clover
    deriving (Show, Eq)

card :: String -> Card
card [v,s] = Card (valueFromChar v) (suitFromChar s)

cards :: String -> [Card]
cards = map card . words

suitFromChar :: Char -> Suit
suitFromChar 's' = Spade
suitFromChar 'h' = Heart
suitFromChar 'd' = Diamond
suitFromChar 'c' = Clover

valueFromChar :: Char -> Value
valueFromChar '2' = Two
valueFromChar '3' = Three
valueFromChar '4' = Four
valueFromChar '5' = Five
valueFromChar '6' = Six
valueFromChar '7' = Seven
valueFromChar '8' = Eight
valueFromChar '9' = Nine
valueFromChar 'T' = Ten
valueFromChar 'J' = Jack
valueFromChar 'Q' = Queen
valueFromChar 'K' = King
valueFromChar 'A' = Ace

suit :: Card -> Suit
suit (Card _ s) = s

value :: Card -> Value
value (Card v _) = v

data Hand = Fold 
          | HighCard [Value]
    deriving (Ord, Eq, Show)

hand :: [Card] -> Hand
hand cs | length cs < 7 = Fold
        | otherwise     = bestHand cs

bestHand :: [Card] -> Hand
bestHand = HighCard . take 5 . reverse . sort . map value

type Score = (Hand, Bool)

scores :: [[Card]] -> [Score]
scores css = let
    hs = map hand css
    best = maximum hs
    in zip hs (map (\h -> h == best && h /= Fold) hs)

process :: String -> String
process text = let
    ps = lines text 
    css = map cards ps
    scs = scores css
    display :: String -> Score -> String
    display s (HighCard _, True) =  s ++ " " ++ "High Card" ++ " (winner)"
    display s (HighCard _, False) = s ++ " " ++ "High Card"
    display s _                   = s 
    in unlines $ zipWith display ps scs
