import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Int, print, (-))

import Gears.Instances

type Peaksearch t u a = (Traversable t, Applicative u, Pointable u, Stateful a u, Supremum a)

compute :: (Traversable t, Monoid a, Supremum a) => t a -> t a
compute = evaluate . max where

	evaluate :: (Traversable t, Monoid a) => State a (t a) -> t a
	evaluate = extract . run % zero

	max :: Peaksearch t u a => t a |-> u
	max xs = xs ->> compare

	compare :: (Applicative t, Stateful a t, Supremum a) => a |-> t
	compare x = modify (x \/) *> current

trapped :: (Applicative t, Traversable t) => t Int -> t Int
trapped ws = volume <$> compute ws <*> ws <*> via @Reverse compute ws where

	volume :: Int -> Int -> Int -> Int
	volume ls x rs = (ls /\ rs) - x

--------------------------------------------------------------------------------

example :: Stack Int
example = insert 2 $ insert 5 $ insert 1 $ insert 2 $ insert 3 $ insert 4 $ insert 7 $ insert 7 $ insert 6 $ empty

main = print . reduce @Int (+) 0 $ trapped example
