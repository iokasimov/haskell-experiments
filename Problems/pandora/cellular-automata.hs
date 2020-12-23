import "base" Control.Concurrent (threadDelay)
import "base" Data.List (take)
import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances ()
import Gears.Utils (stream_to_list)

import Prelude (IO, Char, Int, putStr, print, reverse, (-), (<>))

type Status = Boolean

type Field = Zipper Stream Status

type Neighbours = Status :*: Status :*: Status

start :: Field
start = let desert = repeat False
	in Tap True . TU $ desert :^: desert

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

display :: Int -> Zipper Stream a -> [a]
display n (Tap x (TU (bs :^: fs))) = take n (stream_to_list bs) <> [x] <> reverse (take n $ stream_to_list fs)

instance Monotonic a (Construction Identity a) where
	reduce f r ~(Construct x (Identity xs)) = f x $ reduce f r xs

record :: (Field -> Status) -> Field -> IO ()
record act being = delay *> snapshot *> evolve where

	evolve, snapshot :: IO ()
	evolve = record act $ being =>> act
	snapshot = print $ display 25 being

delay, purge :: IO ()
delay = threadDelay 1000000
purge = putStr "\ESC[2J"

--------------------------------------------------------------------------------

-- type II = Zipper Stream <:.> Zipper Stream
type II = Tap (Delta <:.> Stream) <:.> Tap (Delta <:.> Stream)

instance Covariant II where
	f <$> TU zz = TU $ f <$$> zz

instance Extractable II where
	extract = extract . extract . run

instance Extendable II where
	zz =>> f = f <$> TU (horizontal <$> vertical zz) where

		horizontal, vertical :: II a -> Zipper Stream (II a)
		horizontal z = Tap z . TU $ move (TU . (rotate @Left <$>) . run) z :^: move (TU . (rotate @Right <$>) . run) z
		vertical z = Tap z . TU $ move (TU . rotate @Left . run) z :^: move (TU . rotate @Right . run) z

		move :: (Extractable t, Pointable t) => (a -> a) -> a -> Construction t a
		move f x = extract . deconstruct $ point . f .-+ x

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

type Around = Status -- current
	:*: Status :*: Status -- horizontal
	:*: Status :*: Status -- vertical
	:*: Status :*: Status -- major diagonal
	:*: Status :*: Status -- minor diagonal

around :: II Status -> Around
around z = extract z :*: plane (sub @Left) :*: plane (sub @Right) :*: plane (sub @Up) :*: plane (sub @Down)
	:*: slant (sub @Down) (sub @Left) :*: slant (sub @Up) (sub @Right)
	:*: slant (sub @Up) (sub @Left) :*: slant (sub @Down) (sub @Right) where

	plane :: (Extractable t, Extractable u)
		=> II Status :-. t :. u := Status -> Status
	plane lens = extract . extract $ lens ^. z

	slant :: II Status :-. Stream :. Zipper Stream := Status
		-> Zipper Stream Status :-. Stream Status -> Status
	slant vl hl = extract . view hl . extract . view vl $ z

initial :: II Status
initial = TU . Tap one . TU $ only :^: only where

	only :: Stream :. Zipper Stream := Status
	only = Construct one . Identity $ repeat noone

	one, noone :: Zipper Stream Status
	one = Tap True . TU $ alone :^: alone
	noone = Tap False . TU $ repeat False :^: repeat False

	alone :: Stream Status
	alone = Construct True . Identity $ repeat False

conway :: Around -> Status
conway (focused :*: neighbors) = let count status acc = status ? acc + one $ acc
	in case reduce count zero neighbors of
		Natural (Natural Zero) -> focused
		Natural (Natural (Natural Zero)) -> True
		_ -> False

lifecycle :: (II Status -> Status) -> II Status -> IO ()
lifecycle act being = delay *> purge *> snapshot *> evolve where

	evolve, snapshot :: IO ()
	evolve = lifecycle act $ being =>> act
	snapshot = void $ let screen = display 15
		in screen (screen <$> run being) ->> print

--------------------------------------------------------------------------------

main = lifecycle (conway . around) initial
