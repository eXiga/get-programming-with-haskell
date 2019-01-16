{-# LANGUAGE ScopedTypeVariables #-}

module Chapter18 where

import qualified Data.Map as Map

newtype Box a =
  Box a
  deriving (Show)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box a) = Box $ f a

data Triple a =
  Triple a
         a
         a
  deriving (Show)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

countOrgans :: Map.Map Int Organ -> Map.Map Organ Int
countOrgans catalog = foldr collect initialMap $ Map.elems catalog
  where
    collect k = Map.insertWith (+) k 1
    allOrgans = enumFrom Heart
    initialCount = 0 `replicate` length allOrgans
    initialMap = Map.fromList $ zip allOrgans initialCount
