import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)

import Gears.Instances

data Profit a = Profit a a

instance Semigroup a => Semigroup (Profit a) where
	Profit buy sell + Profit buy' sell' =
		Profit / buy + buy' / sell + sell'

instance Monoid a => Monoid (Profit a) where
	zero = Profit zero zero

instance (Chain a, Group a) => Supremum (Profit a) where
	l@(Profit buy sell) \/ r@(Profit buy' sell') =
		let cheaper = buy <=> buy' & order r l l
		in sell - buy <=> sell' - buy' & order r cheaper l

type Quotes = Nonempty Stack

type Potential a = Stack :. Profit := a

stonks :: forall a . (Chain a, Group a) => Quotes a -> Profit a
stonks prices = top $ prices =>> potential where

	potential :: Quotes a -> Stack := Profit a
	potential remaining = let buy = extract remaining in
		unite $ Profit buy <$$> deconstruct remaining

	top :: Nonempty Stack (Potential a) -> Profit a
	top = reduce (\/) zero . reduce @(Potential a) (+) empty

--------------------------------------------------------------------------------

deriving instance Show a => Show (Profit a)

example :: Quotes Int
example = 9 += 11 += 8 += 5 += 7 += point 10
-- example = insert 10 $ insert 7 $ insert 5 $ insert 8 $ insert 11 $ point 9

main = print $ stonks example
