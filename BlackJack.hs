module BlackJack where
import Cards
import RunGame
import System.Random

import Test.QuickCheck hiding (shuffle)

--A0
{- size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))

Step1: size (Add (Card (Numeric 2) Hearts)) = 1 + size (Add (Card Jack Spades))
Step2: size (Add (Card Jack Spades)) = 1 + size Empty
Step3: size Empty = 0
Step4: Step1 + Step2 + Step3 = 1 + 1 + 0 = 2 -}

--A1
-- | A function that returns an emtpy hand
empty :: Hand
empty = Empty

--A2
-- | Computes the value of all the cards in the hand
value :: Hand -> Integer
value hand
  | initialValue hand > 21 = changeValueAces hand
  | otherwise = initialValue hand

-- | A function that calculates the inital value of the cards in the hand
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueCard card + initialValue hand

-- | A function that changes the value of the aces in case the value of the hand exceeds 21
changeValueAces :: Hand -> Integer
changeValueAces Empty = 0
changeValueAces (Add card hand)
  | (rank card) == Ace = 1 + changeValueAces hand
  | otherwise = valueCard card + changeValueAces hand

-- | A function that returns the value of the rank from the card
valueRank :: Rank -> Integer
valueRank Ace =  11
valueRank (Numeric n) = n
valueRank _ = 10

-- | A funtion that returns the value of the card
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

--A3
-- | A function that checks if the value of the hand is over 21, and thusly the game is over
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

--A4
-- | Function that checks which player won the game
winner :: Hand -> Hand -> Player
winner guestHand bankHand
  | gameOver guestHand = Bank
  | gameOver bankHand = Guest
  | value guestHand > value bankHand = Guest
  | otherwise = Bank

--B1
-- | Function that takes two hands and them on top of each other
(<+) :: Hand -> Hand -> Hand
(<+) hand1 Empty = hand1
(<+) hand1 (Add card Empty) = (Add card hand1)
(<+) hand1 (Add card hand2) = (Add card ((<+) hand1 hand2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
  p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 =
  size p1 + size p2 == size (p1 <+ p2)


hand1 = Add (Card {rank = Numeric 5, suit = Hearts}) (Add (Card {rank = Queen, suit = Diamonds}) (Add (Card {rank = Numeric 9, suit = Clubs}) Empty))

hand2 = Add (Card {rank = Numeric 7, suit = Diamonds}) (Add (Card {rank = Numeric 4, suit = Diamonds}) (Add (Card {rank = Numeric 3, suit = Diamonds})  Empty))

hand3 = Add (Card {rank = Numeric 8, suit = Hearts}) (Add (Card {rank = Numeric 5, suit = Diamonds}) Empty)

hand4 = Empty

hand5 = Add(Card {rank = Numeric 2, suit = Hearts}) (Add (Card {rank = Numeric 2, suit = Hearts}) Empty)

test1 :: Hand -> Hand -> Hand -> Hand
test1 hand1 hand2 hand3 = hand1<+(hand2<+hand3)

test11 :: Hand -> Hand -> Hand -> Hand
test11 hand1 hand2 hand3 = (hand1<+hand2)<+hand3

test2 :: Hand -> Hand -> Hand
test2 hand1 hand2 = hand1<+hand2

--B2
fullHand :: Hand
fullHand = fullSuit Hearts <+
    fullSuit Diamonds <+
    fullSuit Spades <+
    fullSuit Clubs

fullSuit :: Suit -> Hand
fullSuit suit = (Add (Card Ace suit)
  (Add (Card King suit)
  (Add (Card Queen suit)
  (Add (Card Jack suit)
  (Add (Card (Numeric 10) suit)
  (Add (Card (Numeric 9) suit)
  (Add (Card (Numeric 8) suit)
  (Add (Card (Numeric 7) suit)
  (Add (Card (Numeric 6) suit)
  (Add (Card (Numeric 5) suit)
  (Add (Card (Numeric 4) suit)
  (Add (Card (Numeric 3) suit)
  (Add (Card (Numeric 2) suit) Empty)))))))))))))

--B3
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add cardDeck handDeck) hand =
  (handDeck, Add cardDeck hand)

--B4
playBank :: Hand -> Hand
playBank bankHand = playBank' fullHand bankHand

playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand < 16 = playBank' deck' bankHand'
                        | otherwise = bankHand
  where (deck',bankHand') = draw deck bankHand


--B5

shuffle :: StdGen -> Hand -> Hand

mkStdGen :: Int -> StdGen

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
