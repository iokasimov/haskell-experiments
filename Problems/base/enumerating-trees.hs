module Main where

import "base" Data.Foldable (foldrM)
import "base" Data.List (inits, tails)

data Rose a = a :& [Rose a] deriving Show

enumForests :: forall a . [a] -> [[Rose a]]
enumForests = foldrM f [] where

	f :: a -> [Rose a] -> [[Rose a]]
	f x xs = zipWith (\is ts -> (x :& is) : ts) (inits xs) (tails xs)

main = traverse print $ enumForests [1,2]
