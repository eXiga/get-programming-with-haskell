module Chapter7 where

cGcd :: Integral a => a -> a -> a
cGcd a b =
  if remainder == 0
    then b
    else cGcd b remainder
  where
    remainder = a `mod` b

cHead :: [a] -> a
cHead (x:_) = x
cHead [] = error "No head for empty list"

cTail :: [a] -> [a]
cTail (_:xs) = xs
cTail [] = []

improvedGcd :: Integral a => a -> a -> a
improvedGcd a 0 = a
improvedGcd a b = improvedGcd b (a `mod` b)
