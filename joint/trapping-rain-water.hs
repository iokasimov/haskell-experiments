module Main where

import "base" Data.Traversable (for)
import "transformers" Data.Functor.Reverse (Reverse (Reverse))
import "joint" Control.Joint (Stateful, State, current, modify, run)

type Height = Int

walls :: [Height]
walls = [2,5,1,2,3,4,7,7,6]

themax :: (Applicative t, Stateful Height t) => Height -> t Height
themax x = modify (max x) *> current

main = do
	let lms = snd $ run @(State Height) (for walls themax) 0
	let Reverse rms = snd $ run @(State Height) (for (Reverse walls) themax) 0
	print . sum $ zipWith3 (\l x r -> min l r - x) lms walls rms
