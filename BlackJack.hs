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
-- | Function that takes two hands and put them on top of each other
(<+) :: Hand -> Hand -> Hand
(<+) hand1 Empty = hand1
(<+) hand1 (Add card Empty) = Add card hand1
(<+) hand1 (Add card hand2) = (Add card ((<+) hand1 hand2))

-- | A test that checks if the operator <+ works as it should
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
  p1<+(p2<+p3) == (p1<+p2)<+p3

-- | A test that checks that the size of the hands stay the same after using the operator <+
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 =
  size p1 + size p2 == size (p1 <+ p2)

--B2
-- | A function that returns a full stacked deck
fullDeck :: Hand
fullDeck = fullSuit Hearts <+
    fullSuit Diamonds <+
    fullSuit Spades <+
    fullSuit Clubs

-- | A function that returns all the cards in one suit
fullSuit :: Suit -> Hand
fullSuit suit = Add (Card Ace suit)
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
  (Add (Card (Numeric 2) suit) Empty))))))))))))

--B3
-- | A function that draws a card
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add cardDeck handDeck) hand =
  (handDeck, Add cardDeck hand)

--B4
-- | A function that plays for the bank
playBank :: Hand -> Hand
playBank bankHand = playBank' bankHand Empty

-- | A helper function for playBank
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand' < 16 = playBank' deck' bankHand'
                        | otherwise = bankHand'
  where (deck',bankHand') = draw deck bankHand


--B5
-- | A function that shuffles the deck
shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle g deck  = Add card (shuffle g' (notShuffledDeck card deck))
  where (number, g') = randomR (0, size deck - 1) g
        card         = findIndex number deck

-- | A function that finds a given random card
findIndex :: Int -> Hand -> Card
findIndex 0 (Add card hand) = card
findIndex index (Add card hand) = findIndex (index-1) hand

-- | A function that retuns a deck without the random given card from findIndex
notShuffledDeck :: Card -> Hand -> Hand
notShuffledDeck _ Empty = Empty
notShuffledDeck pickedCard (Add card hand) | pickedCard == card = (tempDeck hand)
                                           | otherwise = Add card (notShuffledDeck pickedCard hand)

-- | A function that makes it possible to have two of the same card in one deck without one of them
--    being removed after shuffeling the deck.
tempDeck :: Hand -> Hand
tempDeck Empty = Empty
tempDeck (Add card hand) = Add card (tempDeck hand)

-- | A test that checks that the same cards exist in a deck before and after it is shuffeled.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

-- | A helper function given by the lab instructions, checks if a card exist in a deck.
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- | A test that checks if the size of a deck is the same before and after it is shuffeled
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)

-- | A game interface that makes it possible to run the game.
implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

-- | A function that takes inputs from the user
main :: IO ()
main = runGame implementation
