module DistanceConversions
( yardsToFeet
, feetToInches
, inchesToCentimetres
) where

-- Define yards to feet
yardsToFeet ::  Floating a => a -> a
yardsToFeet = (3*)

-- Define feet to inches
feetToInches :: Floating a => a -> a
feetToInches = (12*)

-- Define inches to centimetres
inchesToCentimetres :: Floating a => a -> a
inchesToCentimetres = (2.54*)
