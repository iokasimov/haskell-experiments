module Main where

import "base" Control.Applicative (Alternative ((<|>)))
import "joint" Control.Joint (Adaptable (adapt), Stateful, State
	, current, replace, run, type (:>), type (:=))

type Prices = [Int]

max_profit :: State Prices :> [] := (Int, Int, Int)
max_profit = current @Prices >>= \case
	[] -> pure (0, 0, 0)
	(buy : rest) ->
		let result sell = (sell - buy, buy, sell) in
		let now = result <$> adapt rest in
		let later = replace rest *> max_profit in
		now <|> later

prices :: Prices
prices = [10, 7, 5, 8, 11, 9]

main = print . maximum $ snd <$> run max_profit prices
