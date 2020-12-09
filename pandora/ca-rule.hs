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

instance Substructure Left (Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)) where
	type Substructural Left (Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)) a = Zipper Stream :. Stream := a
	substructure (run . extract -> Tap x (TU (d :^: u))) =
		let target = Tap (sub @Left ^. x) . TU $ view (sub @Left) <$> d :^: view (sub @Left) <$> u in
		let around lx = (set (sub @Left) <$> sub @Left ^. lx <*> d) :^: (set (sub @Left) <$> sub @Left ^. lx <*> u) in
		Store $ target :*: \lx -> Tag . TU . Tap (sub @Left .~ extract lx $ x) . TU $ around lx

instance Substructure Right (Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)) where
	type Substructural Right (Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)) a = Zipper Stream :. Stream := a
	substructure (run . extract -> Tap x (TU (d :^: u))) =
		let target = Tap (sub @Right ^. x) . TU $ view (sub @Right) <$> d :^: view (sub @Right) <$> u in
		let around rx = (set (sub @Right) <$> sub @Right ^. rx <*> d) :^: (set (sub @Right) <$> sub @Right ^. rx <*> u) in
		Store $ target :*: \rx -> Tag . TU . Tap (sub @Right .~ extract rx $ x) . TU $ around rx

-- horizontal :*: vertical :*: major diagonal :*: minor diagonal
type Around = (Status :*: Status) :*: (Status :*: Status) :*: (Status :*: Status) :*: (Status :*: Status)

around :: II Status -> Around
around z = horizontal :*: vertical :*: major :*: minor where

	horizontal, vertical :: Status :*: Status
	horizontal = plane (sub @Left) :*: plane (sub @Right)
	vertical = plane (sub @Up) :*: plane (sub @Down)

	major, minor :: Status :*: Status
	major = slant (sub @Down) (sub @Left) :*: slant (sub @Up) (sub @Right)
	minor = slant (sub @Up) (sub @Left) :*: slant (sub @Down) (sub @Right)

	plane :: (Extractable t, Extractable u)
		=> II Status :-. t :. u := Status -> Status
	plane lens = extract . extract $ lens ^. z

	slant :: (II Status :-. (Stream :. Zipper Stream := Status))
		-> (Zipper Stream Status :-. Stream Status) -> Status
	slant lens lens' = extract . view lens' . extract . view lens $ z

--------------------------------------------------------------------------------

lifecycle act being = do
	threadDelay 1000000
	print $ display (natural 25) being
	lifecycle act $ being =>> act

main = lifecycle (rule90 . neighbourhood) start
