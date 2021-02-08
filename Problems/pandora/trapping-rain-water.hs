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
	compare x = modify (x \/) *> current

--------------------------------------------------------------------------------

example :: Stack Int
example = 2 += 5 += 1 += 2 += 3 += 4 += 7 += 7 += 6 += empty

main = print . reduce @Int (+) 0 $ trapped example
