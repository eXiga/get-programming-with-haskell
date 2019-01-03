module Lib where

class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe value =
    if value
      then "true"
      else "false"

data IceCream
  = Chocolate
  | Vanilla
  deriving (Show, Eq, Ord)

testIceCreamOrder :: [IceCream]
testIceCreamOrder = [Chocolate, Vanilla, Chocolate]

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == maxBound
    then minBound
    else succ n
