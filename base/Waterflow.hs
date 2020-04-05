module Main where

-- Source: https://chrisdone.com/posts/twitter-problem-loeb/

import Data.Functor
import Control.Applicative.Backwards
import Control.Monad.Trans.State

walls = [2,5,1,2,3,4,7,7,6]

type Current = Int
type LeftMax = Int
type RightMax = Int

leftmax :: Current -> State LeftMax (LeftMax, Current)
leftmax current = get >>= \previous_lm -> case compare previous_lm current of
	EQ -> put current $> (current, current)
	LT -> put current $> (current, current)
	GT -> pure (previous_lm, current)

rightmax :: (LeftMax, Current) -> State RightMax RightMax
rightmax (lm, current) = get >>= \previous_rm -> put (min lm previous_rm) $> min lm previous_rm

main = do
	let (lms, last_value) = runState (traverse leftmax walls) 0
	let (rms, _) = runState (forwards $ traverse (Backwards . rightmax) lms) last_value
	print $ zipWith (\(ml, x) mr -> min ml mr - x) lms rms
