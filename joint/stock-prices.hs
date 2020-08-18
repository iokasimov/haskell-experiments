module Main where

import "joint" Control.Joint (Liftable, Stateful, State, lift, current, modify, run, type (:>), type (:=))

type Prices = [Int]

max_profit :: State Prices :> [] := Int
max_profit = current @Prices >>= \case
	(buy : []) -> pure $ negate buy
	(buy : sales) -> ((+) (negate buy)) <$> lift sales
	[] -> pure 0

prices = [7, 5, 8, 11, 9]

-- [10, 7, 5, 8, 11, 9] ==> [(-3,()),(-5,()),(-2,()),(1,()),(-1,())]
-- [7, 5, 8, 11, 9] ==> [(-2,()),(1,()),(4,()),(2,())]

main = do
	print "Work in progress..."
	print $ run max_profit prices
