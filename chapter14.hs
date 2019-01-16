module Chapter14 where

data NewEngland
  = ME
  | VT
  | NH
  | MA
  | RI
  | CT

instance Show NewEngland where
  show ME = "Maine"
  show VT = "Vermont"
  show NH = "New Hampshire"
  show MA = "Massachusetts"
  show RI = "Rhode Island"
  show CT = "Connecticut"

data SixSidedDice
  = S1
  | S2
  | S3
  | S4
  | S5
  | S6
  deriving (Eq, Ord, Enum)

instance Bounded SixSidedDice where
  minBound = S1
  maxBound = S6

instance Show SixSidedDice where
  show S1 = "•"
  show S2 = "• •"
  show S3 = "• • •"
  show S4 = "• •" ++ "\n" ++ "• •"
  show S5 = "•   •" ++ "\n" ++ "  •  " ++ "\n" ++ "•   •"
  show S6 = "• •" ++ "\n" ++ "• •" ++ "\n" ++ "• •"

class (Eq a, Enum a, Bounded a) =>
      Dice a where
  throw :: a -> a

-- We can use System.Random here, but I decided not to overcomplicate it.
instance Dice SixSidedDice where
  throw d =
    if d == (maxBound :: SixSidedDice)
      then toEnum $ fromEnum (minBound :: SixSidedDice)
      else toEnum $ succ $ fromEnum d
