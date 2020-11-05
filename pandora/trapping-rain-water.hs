import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Int, print)
import qualified Prelude as P

getmax :: (Applicative t, Stateful a t, Supremum a) => a |-> t
getmax x = modify (x \/) *> current

type Peaksearch t u a = (Traversable t, Applicative u, Pointable u, Stateful a u, Supremum a)

themax:: Peaksearch t u a => t a |-> u
themax xs = xs ->> getmax

evaluate :: (Traversable t, Monoid a) => State a (t a) -> t a
evaluate = extract . run % zero

--------------------------------------------------------------------------------

instance Semigroup Int where (+) = (P.+)
instance Monoid Int where zero = 0
instance Infimum Int where (/\) = P.min
instance Supremum Int where (\/) = P.max

walls :: Stack Int
walls = insert 2 $ insert 5 $ insert 1 $ insert 2 $ insert 3 $ insert 4 $ insert 7 $ insert 7 $ insert 6 $ empty

main = do
	let lhs = evaluate . themax $ walls
	let rhs = run . evaluate . themax $ Reverse walls
	let volume l x r = (l /\ r) P.- x
	let result = volume <$> lhs <*> walls <*> rhs
	print . extract . run @(State Int) % 0 $ fold (+) result
