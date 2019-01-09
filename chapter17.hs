module Lib where

import Data.Char (isSpace)
import Data.Semigroup

-- Colors part
data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | Clear
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) Clear a = a
  (<>) a Clear = a
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

instance Monoid Color where
  mempty = Clear
  mappend = (<>)

--
-- Probability table part
--
-- Event type
newtype Event = Event
  { unEvent :: String
  }

instance Semigroup Event where
  (<>) e1 e2
    | isEmpty $ unEvent e1 = Event {unEvent = unEvent e2}
    | isEmpty $ unEvent e2 = Event {unEvent = unEvent e1}
    | isEmpty (unEvent e1) && isEmpty (unEvent e2) = Event {unEvent = ""}
    | otherwise = Event {unEvent = mconcat [unEvent e1, "-", unEvent e2]}
    where
      isEmpty = all isSpace

instance Monoid Event where
  mempty = Event {unEvent = ""}
  mappend = (<>)

-- Probability type
newtype Probability = Probability
  { unProb :: Double
  }

instance Semigroup Probability where
  (<>) p1 p2 = Probability {unProb = unProb p1 + unProb p2}

instance Monoid Probability where
  mempty = Probability {unProb = 0}
  mappend = (<>)

instance Show Probability where
  show p = mconcat [show $ unProb p * 100, "%"]

-- Events type
newtype Events =
  Events [Event]

instance Semigroup Events where
  (<>) (Events e1) (Events []) = Events e1
  (<>) (Events []) (Events e2) = Events e2
  (<>) (Events e1) (Events e2) = Events $ cartesianCombine (<>) e1 e2

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

-- Probs type
newtype Probs =
  Probs [Probability]

instance Semigroup Probs where
  (<>) (Probs p1) (Probs []) = Probs p1
  (<>) (Probs []) (Probs p2) = Probs p2
  (<>) (Probs p1) (Probs p2) = Probs (cartesianCombine (<>) p1 p2)

-- PTable type
data PTable =
  PTable Events
         Probs

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable (e1 <> e2) (p1 <> p2)

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

instance Show PTable where
  show (PTable (Events e) (Probs p)) = mconcat pairs
    where
      pairs = zipWith showPair e p

-- Helpers
createPTable :: Events -> Probs -> PTable
createPTable e (Probs p) = PTable e $ Probs normalizedProbs
  where
    totalProbs = sum $ map unProb p
    normalizedProbs = map (\x -> Probability {unProb = unProb x / totalProbs}) p

showPair :: Event -> Probability -> String
showPair e p = mconcat [unEvent e, "|", show p, "\n"]

cartesianCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianCombine f x y = zipWith f nx cy
  where
    nToAdd = length y
    repeatedX = map (replicate nToAdd) x
    nx = mconcat repeatedX
    cy = cycle y

createCoinsSide :: PTable
createCoinsSide =
  createPTable
    (Events [Event {unEvent = "heads"}, Event {unEvent = "tails"}])
    (Probs [Probability {unProb = 0.5}, Probability {unProb = 0.5}])

createCoinsColor :: PTable
createCoinsColor =
  createPTable
    (Events
       [ Event {unEvent = "red"}
       , Event {unEvent = "blue"}
       , Event {unEvent = "green"}
       ])
    (Probs
       [ Probability {unProb = 0.1}
       , Probability {unProb = 0.2}
       , Probability {unProb = 0.7}
       ])
