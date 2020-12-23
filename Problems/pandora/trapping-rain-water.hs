import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Int, print)
import qualified Prelude as P

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

--------------------------------------------------------------------------------

walls :: Stack Int
walls = insert 2 $ insert 5 $ insert 1 $ insert 2 $ insert 3 $ insert 4 $ insert 7 $ insert 7 $ insert 6 $ empty

main = let volume ls x rs = (ls /\ rs) P.- x in
	print . reduce @Int (+) 0 $ volume <$> compute walls <*> walls <*> via @Reverse compute walls
