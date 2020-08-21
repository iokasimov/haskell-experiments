module Main where

import "base" Control.Applicative (Alternative (empty, (<|>)))
import "joint" Control.Joint (Liftable, Stateful, State, lift, current, modify, run, type (:>), type (:=))

type Prices = [Int]

max_profit :: State Prices :> [] := Int
max_profit = current @Prices >>= \case
	(buy : []) -> pure $ negate buy
	(buy : sales) -> (+) (negate buy) <$> lift sales <|> modify (const sales) *> max_profit
	[] -> pure 0

prices = [10, 7, 5, 8, 11, 9]

main = print . maximum $ snd <$> run max_profit prices
