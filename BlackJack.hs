module BlackJack where
import Cards
import RunGame

import Test.QuickCheck

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



{-Empty <+ Empty = Empty
(Add card1 hand1) <+ Empty = (Add card1 hand1)
Empty <+ (Add card2 hand2) = (Add card2 hand2)
(Add card1 hand1) <+ (Add card2 hand2) =  hand1 <+ (Add card1 hand2)-}



--(<+) (Add card1 hand1) Empty = (Add card1 hand1)
--(<+) Empty (Add card2 hand2) = (Add card2 hand2) <+ hand1 --Remove <+ hand2
--(<+) (Add card1 hand1) (Add card2 hand2) = (Add card1 (Add card2 hand1)) <+ hand2 --Continue Here

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
