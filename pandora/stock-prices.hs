import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print, (-), compare)
import qualified Prelude as P

type Prices = Nonempty Stack Int

maximum :: (Monotonic (t a) a, Monoid a, Supremum a) => t a -> a
maximum = reduce (\/) zero

data Transaction = Transaction Int Int deriving Show

instance Semigroup Transaction where
	Transaction buy sell + Transaction buy' sell' =
		Transaction (buy + buy') (sell + sell')

instance Monoid Transaction where
	zero = Transaction 0 0

instance Supremum Transaction where
	Transaction buy sell \/ Transaction buy' sell' =
		case compare (sell - buy) (sell' - buy') of
			P.GT -> Transaction buy sell
			P.EQ -> Transaction buy sell
			P.LT -> Transaction buy' sell'

type Potentials = Stack Transaction

stonks :: Prices -> Transaction
stonks prices = top $ prices =>> potential where

	potential :: Prices -> Stack := Transaction
	potential remaining = TU $ Transaction (extract remaining) <$$> deconstruct remaining

	top :: Construction Maybe Potentials -> Transaction
	top = maximum . reduce @_ @Potentials (+) empty

--------------------------------------------------------------------------------

example :: Prices
example = insert 9 $ insert 11 $ insert 8 $ insert 5 $ insert 7 $ point 10
-- example = insert 10 $ insert 7 $ insert 5 $ insert 8 $ insert 11 $ point 9

listify :: [a] -> Stack a -> [a]
listify r (TU (Just (Construct x next))) = listify (x : r) $ TU next
listify r (TU Nothing) = r

instance Covariant [] where
	f <$> [] = []
	f <$> (x : xs) = (f x) : (f <$> xs)

instance Traversable [] where
	[] ->> _ = point []
	(x : xs) ->> f = (:) <$> f x <*> xs ->> f

deriving instance (Show a, Show b) => Show (a :*: b)
deriving instance Show a => Show (Maybe a)

instance Semigroup Int where (+) = (P.+)
instance Monoid Int where zero = 0
instance Infimum Int where (/\) = P.min
instance Supremum Int where (\/) = P.max

main = print $ stonks example
