import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where
        fmap f Empty = Empty
        fmap f (Value x lst) = Value (f x) (fmap f lst)


-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty b = b
combineLists a Empty = a
combineLists (Value x Empty) lst = (Value x lst) -- this is the cons.
combineLists (Value x lst1) lst2 = Value x (combineLists lst1 lst2)

-- Make our list a Monoid

-- Make our list an Applicative

instance Applicative List where
    pure f = Value f Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Value f fs) <*> lst = combineLists (fmap f lst) (fs <*> lst)

-- Make sure that the List obeys the laws for Applicative and Monoid
-- doing this part on paper

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty
oneValueList = Value 3 Empty
emptyList = Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)

s1 = plusTwo <$> emptyList
s2 = plusTwo <$> oneValueList
s3 = plusTwo <$> twoValueList

-- Use <$> and <*> on the lists with a binary function

b = mod  <$> oneValueList <*> twoValueList
be1 = mod  <$> Empty  <*> twoValueList
be2 = mod  <$> oneValueList <*> Empty

-- Create some lists of binary functions

allTwos = Value plusTwo $ Value (subtract 2) $ Value (\x -> div x 2) $ Value (*2) Empty

-- Use <*> on the binary functions list and the number lists

t = allTwos <*> (Value 1 $ Value 10 $ Value 100 $ Value 1000 Empty)
