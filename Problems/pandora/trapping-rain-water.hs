import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Int, print)

import Gears.Instances

trapped :: forall a t . (Applicative t, Traversable t, Group a, Infimum a, Supremum a) => t a -> t a
trapped walls = volume <$> peak walls <*> walls <*> peak @(Reverse t) -=: walls where

	volume :: a -> a -> a -> a
	volume ls x rs = (ls /\ rs) - x

	peak :: Traversable v => v a -> v a
	peak columns = extract . run % zero $ columns ->> compare

	compare :: a :=> State a
	compare x = modify (x \/)

--------------------------------------------------------------------------------

example :: List Int
example = item @Push 2 $ item @Push 5 $ item @Push 1
	$ item @Push 2 $ item @Push 3 $ item @Push 4
	$ item @Push 7 $ item @Push 7 $ item @Push 6 $ empty

main = print . reduce @Int (+) 0 $ trapped example
