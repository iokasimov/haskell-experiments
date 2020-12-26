import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)

import Gears.Instances

type Prices = Nonempty Stack Int

type Potential = Stack Profit

stonks :: Prices -> Profit
stonks prices = top $ prices =>> potential where

	potential :: Prices -> Stack := Profit
	potential remaining = let buy = extract remaining in
		unite $ Profit buy <$$> deconstruct remaining

	top :: Nonempty Stack Potential -> Profit
	top = reduce (\/) zero . reduce @Potential (+) empty

data Profit = Profit Int Int

instance Semigroup Profit where
	Profit buy sell + Profit buy' sell' =
		Profit (buy + buy') (sell + sell')

instance Monoid Profit where
	zero = Profit 0 0

instance Supremum Profit where
	l@(Profit buy sell) \/ r@(Profit buy' sell') =
		let cheaper = buy <=> buy' & order r l l
		in sell - buy <=> sell' - buy' & order r cheaper l

--------------------------------------------------------------------------------

deriving instance Show Profit

example :: Prices
example = insert 9 $ insert 11 $ insert 8 $ insert 5 $ insert 7 $ point 10
-- example = insert 10 $ insert 7 $ insert 5 $ insert 8 $ insert 11 $ point 9

main = print $ stonks example
