{-
 - We are going to create some types for a deck of cards
 - The cards need to have an ordering, based on the standard ranking http://en.wikipedia.org/wiki/Standard_52-card_deck#Rank_and_color
 - We are assuming Aces are high.
 - Therefore, the following statements should be true:
 -    (Card Ace Spades) > (Card King Spades)
 -    (Card Two Clubs) < (Card Three Clubs)
 -
 - We are going to provide our own implementation of the Show typeclass for the Card type.
 - When displaying the Card instance in GHCI, or calling show (Card digit suit), the String which should be displayed is "The <Digit> of <Suit>"
 -
 - Uncomment the following declarations to complete the implementation, and provide an implementation for instance Show Card
 -}

data Suit = Hearts | Diamonds | Clubs | Spades 
        deriving (Eq, Show)
data Face = Jack | Queen | King | Ace
        deriving (Eq, Ord, Enum, Show)
data Digit = NumberCard Int | FaceCard Face
        deriving (Show,Eq)
data Card = Card Digit Suit
        deriving (Show,Eq)

-- We should be able to provide a function which returns the higher ranked card:
betterCard :: Card -> Card -> Card
betterCard x@(Card (NumberCard _) _) y@(Card (FaceCard _) _ ) = y
betterCard x@(Card (NumberCard xnum) _) y@(Card (NumberCard ynum) _ ) = if (ynum>xnum) then y else x
betterCard x@(Card (FaceCard xface) _) y@(Card (FaceCard yface) _ ) = if (yface>xface) then y else x
betterCard x y = betterCard y x -- if  nothing matches flip the order and try again

-- Here is a new Typeclass, which represents some kind of playing hand in a game.
-- It returns True for a "winning hand", depending on the rules for the type of class we are playing with
class Hand a where
    play :: [a] -> Bool

-- Implement Hand for Card, where play returns true if the list contains the Ace of Spades
instance Hand Card where
    play = (Card (FaceCard Ace) Spades) `elem` 


-- Create a new Coin type
type Coin = Bool

-- Implement Hand for Coin, where play returns true if there are ten heads in a row in the list
instance Hand Coin where
	play c = (take 10 c) == (replicate 10 (True))

-- Have a play with implementing Hand for some other types, for instance Int and Bool
