-- {-# LANGUAGE UndecidableInstances #-}

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

start :: Zipper Stream Status
start = let desert = repeat False
	in Tap True . T_U $ desert :*: desert

rule90 :: Neighbours -> Status
rule90 (True :*: True :*: True) = False
rule90 (True :*: True :*: False) = True
rule90 (True :*: False :*: True) = False
rule90 (True :*: False :*: False) = True
rule90 (False :*: True :*: True) = True
rule90 (False :*: True :*: False) = False
rule90 (False :*: False :*: True) = True
rule90 (False :*: False :*: False) = False

neighbourhood :: Zipper Stream Status -> Neighbours
neighbourhood z = extract (view (focus @Left) . run $ lower z) :*: extract z :*: extract (view (focus @Right) . run $ lower z)

display :: Int -> Zipper Stream a -> [a]
display n (Tap x (T_U (bs :*: fs))) = reverse (take n $ stream_to_list bs) <> [x] <> take n (stream_to_list fs)

instance Monotonic a (Construction Identity a) where
	reduce f r ~(Construct x (Identity xs)) = f x $ reduce f r xs

record :: (Zipper Stream Status -> Status) -> Zipper Stream Status -> IO ()
record act being = delay *> snapshot *> evolve where

	evolve, snapshot :: IO ()
	evolve = record act $ being =>> act
	snapshot = print $ display 25 being

delay, purge :: IO ()
delay = threadDelay 1000000
purge = putStr "\ESC[2J"

--------------------------------------------------------------------------------

-- type II = Zipper Stream <:.> Zipper Stream
type II = Tap ((:*:) <:.:> Stream) <:.> Tap ((:*:) <:.:> Stream)

instance Extendable II where
	zz =>> f = f <$> TU (horizontal <$> vertical zz) where

		horizontal, vertical :: II a -> Zipper Stream (II a)
		horizontal z = Tap z . T_U $ move ((morph @(Rotate Left) <$>) ||=) z :*: move ((morph @(Rotate Right) <$>) ||=) z
		vertical z = Tap z . T_U $ move (morph @(Rotate Left) ||=) z :*: move (morph @(Rotate Right) ||=) z

		move :: (Extractable t, Pointable t) => (a -> a) -> a -> Construction t a
		move f x = extract . deconstruct $ point . f .-+ x

instance Substructure Down (Tap ((:*:) <:.:> Stream) <:.> Tap ((:*:) <:.:> Stream)) where
	type Substructural Down (Tap ((:*:) <:.:> Stream) <:.> Tap ((:*:) <:.:> Stream)) = Stream <:.> Zipper Stream
	substructure (run . extract . run -> Tap focused (T_U (d :*: u))) = Store $ TU d :*: lift . TU . Tap focused . T_U . (:*: u) . run

instance Substructure Up (Tap ((:*:) <:.:> Stream) <:.> Tap ((:*:) <:.:> Stream)) where
	type Substructural Up (Tap ((:*:) <:.:> Stream) <:.> Tap ((:*:) <:.:> Stream)) = Stream <:.> Zipper Stream
	substructure (run . extract . run -> Tap focused (T_U (d :*: u))) = Store $ TU u :*: lift . TU . Tap focused . T_U . (d :*:) . run

instance Covariant t => Substructure Left ((:*:) <:.:> t) where
	type Substructural Left ((:*:) <:.:> t) = t
	substructure (run . extract . run -> l :*: r) = Store $ l :*: lift . T_U . (:*: r)

instance Covariant t => Substructure Right ((:*:) <:.:> t) where
	type Substructural Right ((:*:) <:.:> t) = t
	substructure (run . extract . run -> l :*: r) = Store $ r :*: lift . T_U . (l :*:)

instance Substructure Left (Tap ((:*:) <:.:> Stream) <:.> Tap ((:*:) <:.:> Stream)) where
	type Substructural Left (Tap ((:*:) <:.:> Stream) <:.> Tap ((:*:) <:.:> Stream)) = Zipper Stream <:.> Stream
	substructure (run . extract . run -> Tap x (T_U (d :*: u))) = let left = sub @Tail |> sub @Left in
		let target = TU . Tap (view left x) . T_U $ view left <$> d :*: view left <$> u in
		let around lx = set left <$> view left lx <*> d :*: set left <$> view left lx <*> u in
		Store $ target :*: \lx -> lift . TU . Tap (set left (extract $ run lx) x) . T_U $ around (run lx)

instance Substructure Right (Tap ((:*:) <:.:> Stream) <:.> Tap ((:*:) <:.:> Stream)) where
	type Substructural Right (Tap ((:*:) <:.:> Stream) <:.> Tap ((:*:) <:.:> Stream)) = Zipper Stream <:.> Stream
	substructure (run . extract . run -> Tap x (T_U (d :*: u))) = let right = sub @Tail |> sub @Right in
		let target = TU . Tap (view right x) . T_U $ view right <$> d :*: view right <$> u in
		let around rx = set right <$> view right rx <*> d :*: set right <$> view right rx <*> u in
		Store $ target :*: \rx -> lift . TU . Tap (set right (extract $ run rx) x) . T_U $ around (run rx)

type Around = Status -- current
	:*: Status :*: Status -- horizontal
	:*: Status :*: Status -- vertical
	:*: Status :*: Status -- major diagonal
	:*: Status :*: Status -- minor diagonal

around :: II Status -> Around
around z = extract z :*: plane (sub @Left) :*: plane (sub @Right) :*: plane (sub @Up) :*: plane (sub @Down)
	:*: slant (sub @Down) (sub @Tail |> sub @Left) :*: slant (sub @Up) (sub @Tail |> sub @Right)
	:*: slant (sub @Up) (sub @Tail |> sub @Left) :*: slant (sub @Down) (sub @Tail |> sub @Right) where

	plane :: (Extractable t, Extractable u) => II :~. (t <:.> u) -> Status
	plane lens = extract . extract . run . view lens $ z

	slant :: II :~. (Stream <:.> Zipper Stream) -> (Zipper Stream :~. Stream) -> Status
	slant vl hl = extract . view hl . extract . run . view vl $ z

conway :: Around -> Status
conway (focused :*: neighbors) = let count status acc = status ? acc + one $ acc
	in case reduce count Zero neighbors of
		Numerator (Denumerator One) -> focused
		Numerator (Denumerator (Denumerator One)) -> True
		_ -> False

lifecycle :: (II Status -> Status) -> II Status -> IO ()
lifecycle act being = delay *> purge *> snapshot *> evolve where

	evolve, snapshot :: IO ()
	evolve = lifecycle act $ being =>> act
	snapshot = void $ let screen = display 15
		in screen (screen <$> run being) ->> print

--------------------------------------------------------------------------------

cube :: II Status
cube = TU . Tap one . T_U $ only :*: only where

	only :: Stream :. Zipper Stream := Status
	only = Construct one . Identity $ repeat noone

	one, noone :: Zipper Stream Status
	one = Tap True . T_U $ alone :*: alone
	noone = Tap False . T_U $ repeat False :*: repeat False

	alone :: Stream Status
	alone = Construct True . Identity $ repeat False

blinker :: II Status
blinker = TU . Tap one . T_U $ repeat noone :*: repeat noone where

	one, noone :: Zipper Stream Status
	one = Tap True . T_U $ alone :*: alone
	noone = Tap False . T_U $ repeat False :*: repeat False

	alone :: Stream Status
	alone = Construct True . Identity $ repeat False

main = lifecycle (conway . around) cube
