import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import GHC.Int (Int, eqInt)
import Prelude (Char, Show (show), print, reverse, (-), (<>))
import Control.Concurrent (threadDelay)

type Status = Boolean

type Field = Zipper Stream Status

type Neighbours = Status :*: Status :*: Status

start :: Field
start = let desert = repeat False
	in Tap True . TU $ desert :^: desert

-- rule50 :: Neighbours -> Status
-- rule50 (True :*: True :*: True) = False
-- rule50 (True :*: True :*: False) = False
-- rule50 (True :*: False :*: True) = True
-- rule50 (True :*: False :*: False) = True
-- rule50 (False :*: True :*: True) = False
-- rule50 (False :*: True :*: False) = False
-- rule50 (False :*: False :*: True) = True
-- rule50 (False :*: False :*: False) = False

rule90 :: Neighbours -> Status
rule90 (True :*: True :*: True) = False
rule90 (True :*: True :*: False) = True
rule90 (True :*: False :*: True) = False
rule90 (True :*: False :*: False) = True
rule90 (False :*: True :*: True) = True
rule90 (False :*: True :*: False) = False
rule90 (False :*: False :*: True) = True
rule90 (False :*: False :*: False) = False


neighbourhood :: Field -> Neighbours
neighbourhood z = extract (sub @Left ^. z) :*: extract z :*: extract (sub @Right ^. z)

display :: Natural -> Field -> [Status]
display n (Tap x (TU (bs :^: fs))) = take_n n bs [] <> [x] <> reverse (take_n n fs []) where

	take_n :: Natural -> Stream a -> [a] -> [a]
	take_n (Natural n) (Construct x (Identity next)) r = take_n n next $ x : r
	take_n Zero _ r = r

instance Show Boolean where
	show True = "*"
	show False = " "

instance Setoid Int where
	x == y = if eqInt x y then True else False

natural :: Int -> Natural
natural n = n == 0 ? Zero $ Natural . natural $ n - 1

instance Monotonic (Construction Identity a) a where
	bypass f r ~(Construct x (Identity xs)) = f x $ bypass f r xs

--------------------------------------------------------------------------------

-- type II = Zipper Stream <:.> Zipper Stream
type II = Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)

instance Covariant II where
	f <$> TU zz = TU $ f <$$> zz

instance Extractable II where
	extract = extract . extract . run

instance Extendable II where
	zz =>> f = f <$> TU (plane <$> plane zz) where

		plane :: II a -> Zipper Stream (II a)
		plane z = TU <$> divergence (run z)

		divergence :: Zipper Stream := a -> Zipper Stream :. Zipper Stream := a
		divergence x = Tap x . TU $ move (rotate @Left) x :^: move (rotate @Right) x

		move :: (Extractable t, Pointable t) => (a -> a) -> a -> Construction t a
		move act = extract . deconstruct . iterate (point . act)

instance Substructure Down (Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)) where
	type Substructural Down (Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)) a = Stream :. Zipper Stream := a
	substructure (run . extract -> Tap focused (TU (down :^: up))) = Store $ down :*: Tag . TU . Tap focused . TU . (:^: up)

instance Substructure Up (Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)) where
	type Substructural Up (Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)) a = Stream :. Zipper Stream := a
	substructure (run . extract -> Tap focused (TU (down :^: up))) = Store $ up :*: Tag . TU . Tap focused . TU . (down :^:)

-- (left :*: right) :*: (up :*: down) :*: major diagonal :*: minor diagonal
type Around = (Status :*: Status) :*: (Status :*: Status) :*: (Status :*: Status) :*: (Status :*: Status)

--------------------------------------------------------------------------------

lifecycle act being = do
	threadDelay 1000000
	print $ display (natural 25) being
	lifecycle act $ being =>> act

main = lifecycle (rule90 . neighbourhood) start
