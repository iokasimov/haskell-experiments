module Main where

import "base" Control.Applicative (Alternative (empty, (<|>)))
import "joint" Control.Joint (Adaptable (adapt), Stateful, State, current, modify, run, type (:>), type (:=))

type Prices = [Int]

max_profit :: State Prices :> [] := Int
max_profit = current @Prices >>= \case
	(buy : []) -> pure $ negate buy
	(buy : sales) -> (+) (negate buy) <$> adapt sales <|> modify (const sales) *> max_profit
	[] -> pure 0

prices :: Prices
prices = [10, 7, 5, 8, 11, 9]

main = print . maximum $ snd <$> run max_profit prices
