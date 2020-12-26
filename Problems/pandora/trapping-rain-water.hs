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

	compare :: a |-> State a
	compare x = modify (x \/) *> current

--------------------------------------------------------------------------------

example :: Stack Int
example = insert 2 $ insert 5 $ insert 1 $ insert 2 $ insert 3 $ insert 4 $ insert 7 $ insert 7 $ insert 6 $ empty

main = print . reduce @Int (+) 0 $ trapped example
