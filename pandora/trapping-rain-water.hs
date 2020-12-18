import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Int, print)
import qualified Prelude as P

type Peaksearch t u a = (Traversable t, Applicative u, Pointable u, Stateful a u, Supremum a)

compute :: (Traversable t, Monoid a, Supremum a) => t a -> t a
compute = evaluate . themax where

	evaluate :: (Traversable t, Monoid a) => State a (t a) -> t a
	evaluate = extract . run % zero

	themax:: Peaksearch t u a => t a |-> u
	themax xs = xs ->> getmax

	getmax :: (Applicative t, Stateful a t, Supremum a) => a |-> t
	getmax x = modify (x \/) *> current

--------------------------------------------------------------------------------

via :: (Liftable t, Interpreted (t u), Interpreted (t v), Covariant u)
	=> (t u a -> t v b) -> u a -> Primary (t v) b
via f = run . f . lift

instance Semigroup Int where (+) = (P.+)
instance Monoid Int where zero = 0
instance Infimum Int where (/\) = P.min
instance Supremum Int where (\/) = P.max

walls :: Stack Int
walls = insert 2 $ insert 5 $ insert 1 $ insert 2 $ insert 3 $ insert 4 $ insert 7 $ insert 7 $ insert 6 $ empty

main = let volume ls x rs = (ls /\ rs) P.- x in
	print . reduce @(Stack Int) @Int (+) 0 $ volume <$> compute walls <*> walls <*> via @Reverse compute walls
