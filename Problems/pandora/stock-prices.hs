import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print, (-), compare)
import qualified Prelude as Base (Ordering (GT, EQ, LT))

import Gears.Instances

type Prices = Nonempty Stack Int

type Potential = Stack Closed

stonks :: Prices -> Closed
stonks prices = top $ prices =>> potential where

	potential :: Prices -> Stack := Closed
	potential remaining = let buy = extract remaining in
		unite $ Closed buy <$$> deconstruct remaining

	top :: Nonempty Stack Potential -> Closed
	top = reduce (\/) zero . reduce @Potential (+) empty

data Closed = Closed Int Int

instance Semigroup Closed where
	Closed buy sell + Closed buy' sell' =
		Closed (buy + buy') (sell + sell')

instance Monoid Closed where
	zero = Closed 0 0

instance Supremum Closed where
	Closed buy sell \/ Closed buy' sell' =
		case compare (sell - buy) (sell' - buy') of
			Base.GT -> Closed buy sell
			Base.EQ -> Closed buy sell
			Base.LT -> Closed buy' sell'

--------------------------------------------------------------------------------

deriving instance Show Closed

example :: Prices
example = insert 9 $ insert 11 $ insert 8 $ insert 5 $ insert 7 $ point 10
-- example = insert 10 $ insert 7 $ insert 5 $ insert 8 $ insert 11 $ point 9

main = print $ stonks example
