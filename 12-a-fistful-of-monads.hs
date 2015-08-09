{-
 - Create a type called Validation
 - The type constructor takes one parameter
 - There are two Values: 
 -   Success, which takes that parameter and
 -   Fail String, which represents a failure, and a reason for that failure
 -}

data Validation a = Success a | Fail String deriving (Eq, Show)

instance Functor Validation where
    fmap f (Success x) = Success (f x)
    fmap f (Fail str) = Fail str

instance Applicative Validation where
    pure = Success
    Fail str <*> _ = Fail str
    _ <*> Fail str = Fail str
    Success a <*> Success b = Success (a b)

-- Make the Validation a Monad
instance Monad Validation where
    return = Success
    Fail str >>= _ = Fail str
    Success x >>= f = f x
    fail = Fail 


{-
 - Create a function, positiveCheck, which takes a number and returns a successful Validation if it's positive, 
 - and a failed Validation with a String message if not.
 -}


positiveCheck :: (Num a, Ord a) => a -> Validation a
positiveCheck x
    | x > 0 = Success x
    | otherwise = Fail "not positive"

{-
 - Create a function, evenCheck, which returns a successful Validation if it's even,
 - and a failed Validation with a string message if it's odd
 -}
evenCheck :: (Integral a)  =>  a -> Validation a
evenCheck x
    | even x = Success x
    | otherwise = Fail "not even"

{-
 - Write a function which uses positiveCheck and evenCheck to make sure a number is both positive and even
 -}

positiveAndEvenCheck :: (Num a, Ord a, Integral a) => a -> Validation a
positiveAndEvenCheck x = (return x) >>= positiveCheck >>= evenCheck
