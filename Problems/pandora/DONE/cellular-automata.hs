{-# LANGUAGE UndecidableInstances #-}

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Control.Concurrent (threadDelay)
import "base" Data.List (take)

import Gears.Instances ()
import Gears.Utils (stream_to_list)

import Prelude (IO, Char, Int, putStr, print, reverse, (-), (<>))

type Status = Boolean

type Field = Zipper Stream Status

type Neighbours = Status :*: Status :*: Status

start :: Zipper Stream Status
start = let desert = repeat False in Tap True $ twosome desert desert

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
neighbourhood z = extract (view # focus @Left $ run # lower z)
	:*: extract z :*: extract (view # focus @Right $ run # lower z)

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

type II = Tap Sides <:.> Tap Sides

type Sides = Stream <:.:> Stream := (:*:)

type Horizontally = Zipper Stream <:.> Stream
type Vertically = Stream <:.> Zipper Stream

instance Extendable II where
	zz =>> f = f <$> TU (horizontal <$> vertical zz) where

		horizontal, vertical :: II a -> Zipper Stream (II a)
		horizontal z = Tap z $ twosome # move ((rotate @Left <$>) ||=) z # move ((rotate @Right <$>) ||=) z
		vertical z = Tap z $ twosome # move (rotate @Left ||=) z # move (rotate @Right ||=) z

		move :: (Extractable t, Pointable t) => (a -> a) -> a -> Construction t a
		move f x = extract . deconstruct $ point . f .-+ x

instance Substructure Down II where
	type Substructural Down II = Vertically
	substructure = PQ_ $ \ii -> case run . extract . run # ii of
		Tap focused (T_U (d :*: u)) -> Store $ TU d :*: lift . TU . Tap focused . (twosome % u) . run

instance Substructure Up II where
	type Substructural Up II = Vertically
	substructure = PQ_ $ \x -> case run . extract . run # x of
		Tap focused (T_U (d :*: u)) -> Store $ TU u :*: lift . TU . Tap focused . twosome d . run

instance Covariant t => Substructure Left (t <:.:> t := (:*:)) where
	type Substructural Left (t <:.:> t := (:*:)) = t
	substructure = PQ_ (Store . (lift .#.. (twosome %) <$>) . run . extract . run)

instance Covariant t => Substructure Right (t <:.:> t := (:*:)) where
	type Substructural Right (t <:.:> t := (:*:)) = t
	substructure = PQ_ (Store . (lift .#.. twosome <$>) . swap . run . extract . run)

instance Substructure Left II where
	type Substructural Left II = Horizontally
	substructure = PQ_ $ \x -> case run . extract . run # x of
		Tap x (T_U (d :*: u)) -> let left = sub @Left . sub @Tail in
			let target = TU $ Tap # view left x $ twosome # view left <$> d # view left <$> u in
			let around lx = twosome # set left <$> view left lx <*> d # set left <$> view left lx <*> u in
			Store $ target :*: \lx -> lift . TU . Tap (set left # extract (run lx) # x) $ around # run lx

instance Substructure Right II where
	type Substructural Right II = Horizontally
	substructure = PQ_ $ \x -> case run . extract . run # x of
		Tap x (T_U (d :*: u)) -> let right = sub @Right . sub @Tail in
			let target = TU . Tap (view right x) $ twosome # view right <$> d # view right <$> u in
			let around rx = twosome # set right <$> view right rx <*> d # set right <$> view right rx <*> u in
			Store $ target :*: \rx -> lift . TU . Tap (set right # extract (run rx) # x) $ around # run rx

type Around = Status -- current
	:*: Status :*: Status -- horizontal
	:*: Status :*: Status -- vertical
	:*: Status :*: Status -- major diagonal
	:*: Status :*: Status -- minor diagonal

around :: II Status -> Around
around z = extract z :*: plane @Left :*: plane @Right :*: plane @Up :*: plane @Down
	:*: slant @Down @Tail @Left :*: slant @Up @Tail @Right :*: slant @Up @Tail @Left :*: slant @Down @Tail @Right where

	plane :: forall i t u . (Substructured i II (t <:.> u), Extractable t, Extractable u) => Status
	plane = extract . extract . run . view (sub @i) $ z

	slant :: forall v q h . (Substructured v II Vertically, Substructured q (Zipper Stream) Sides, Substructured h Sides Stream) => Status
	slant = extract . view (sub @h . sub @q) . extract . run . view (sub @v) $ z

conway :: Around -> Status
conway (focused :*: neighbors) = alive == one + one ? focused
	$ alive == one + one + one ? True $ False where

	alive :: Int
	alive = let count status acc = status ? acc + one $ acc in
		reduce count zero neighbors

lifecycle :: (II Status -> Status) -> II Status -> IO ()
lifecycle act being = delay *> purge *> snapshot *> evolve where

	evolve, snapshot :: IO ()
	evolve = lifecycle act $ being =>> act
	snapshot = void $ let screen = display 5
		in screen (screen <$> run being) ->> print

--------------------------------------------------------------------------------

cube :: II Status
cube = TU . Tap one $ twosome only only where

	only :: Stream :. Zipper Stream := Status
	only = Construct one . Identity $ repeat noone

	one, noone :: Zipper Stream Status
	one = Tap True $ twosome alone alone
	noone = Tap False $ twosome # repeat False # repeat False

	alone :: Stream Status
	alone = Construct True . Identity $ repeat False

blinker :: II Status
blinker = TU . Tap one $ twosome # repeat noone # repeat noone where

	one, noone :: Zipper Stream Status
	one = Tap True $ twosome alone alone
	noone = Tap False $ twosome # repeat False # repeat False

	alone :: Stream Status
	alone = Construct True . Identity $ repeat False

main = lifecycle # conway . around # cube
