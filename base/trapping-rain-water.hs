module Main where

-- Source: https://chrisdone.com/posts/twitter-problem-loeb/

import "base" Data.Traversable
import "transformers" Data.Functor.Reverse
import "transformers" Control.Monad.Trans.State

type Current = Int
type Max = Int

walls :: [Current]
walls = [2,5,1,2,3,4,7,7,6]

themax :: Current -> State Max Max
themax x = modify (max x) *> get

main = do
	let lms = evalState (for walls themax) 0
	let Reverse rms = evalState (for (Reverse walls) themax) 0
	print . sum $ zipWith3 (\l x r -> min l r - x) lms walls rms
