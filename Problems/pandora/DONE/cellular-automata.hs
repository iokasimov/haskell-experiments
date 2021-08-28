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

type Field = Zipper Stream (Left ::: Right) Status

type Neighbours = Status :*: Status :*: Status

start :: Zipper Stream (Left ::: Right) Status
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

neighbourhood :: Zipper Stream (Left ::: Right) Status -> Neighbours
neighbourhood z = extract (extract $ view # sub @Left # lower z)
	:*: extract z :*: extract (extract $ view # sub @Right # lower z)

display :: Int -> Zipper Stream (Left ::: Right) a -> [a]
display n (Tap x (T_U (bs :*: fs))) = reverse (take n $ stream_to_list bs) <> [x] <> take n (stream_to_list fs)

record :: (Zipper Stream (Left ::: Right) Status -> Status) -> Zipper Stream (Left ::: Right) Status -> IO ()
record act being = delay *>- snapshot *>- evolve where

	evolve, snapshot :: IO ()
	evolve = record act $ act <<= being
	snapshot = print $ display 25 being

delay, purge :: IO ()
delay = threadDelay 1000000
purge = putStr "\ESC[2J"

--------------------------------------------------------------------------------

type II = Tap Sides <:.> Tap Sides

type Sides = Stream <:.:> Stream := (:*:)

type Horizontally = Zipper Stream (Left ::: Right) <:.> Stream

-- TODO: There we should use Up :: Down instead of Left ::: Right, but let's change it later
type Vertically = Stream <:.> Zipper Stream (Left ::: Right)

instance Extendable (->) II where
	f <<= zz = f -<$>- TU (horizontal -<$>- vertical zz) where

		horizontal, vertical :: II a -> Zipper Stream (Left ::: Right) (II a)
		horizontal z = Tap z $ twosome # move ((rotate @Left -<$>-) ||=) z # move ((rotate @Right -<$>-) ||=) z
		vertical z = Tap z $ twosome # move (rotate @Left ||=) z # move (rotate @Right ||=) z

		move :: (Extractable_ t, Covariant (->) (->) t, Monoidal (->) (->) (:*:) (:*:) t) => (a -> a) -> a -> Construction t a
		move f x = extract . deconstruct $ point . f .-+ x

instance Substructure Down II where
	type Available Down II = Identity
	type Substance Down II = Vertically
	substructure = P_Q_T $ \ii -> case run . extract . run # ii of
		Tap focused (T_U (d :*: u)) -> Store $ Identity (TU d) :*: lift . TU . Tap focused . (twosome % u) . run . extract

instance Substructure Up II where
	type Available Up II = Identity
	type Substance Up II = Vertically
	substructure = P_Q_T $ \x -> case run . extract . run # x of
		Tap focused (T_U (d :*: u)) -> Store $ Identity (TU u) :*: lift . TU . Tap focused . twosome d . run . extract

instance Substructure Left II where
	type Available Left II = Identity
	type Substance Left II = Horizontally
	substructure = P_Q_T $ \ii ->
		let target = (extract . view (sub @Left) -<$>-) ||= lower ii in
		let updated new = set (sub @Left) . Identity -<$>- new -<*>- run (lower ii) in
		Store $ Identity target :*: lift . (updated ||=) . extract

instance Substructure Right II where
	type Available Right II = Identity
	type Substance Right II = Horizontally
	substructure = P_Q_T $ \ii ->
		let target = (extract . view (sub @Right) -<$>-) ||= lower ii in
		let updated new = set (sub @Right) . Identity -<$>- new -<*>- run (lower ii) in
		Store $ Identity target :*: lift . (updated ||=) . extract

type Around = Status -- current
	:*: Status :*: Status -- horizontal
	:*: Status :*: Status -- vertical
	:*: Status :*: Status -- major diagonal
	:*: Status :*: Status -- minor diagonal

around :: II Status -> Around
around z = extract z :*: plane @Left :*: plane @Right :*: plane @Up :*: plane @Down
	:*: slant @Down @Left :*: slant @Up @Right :*: slant @Up @Left :*: slant @Down @Right where

	plane :: forall i t u . (Substructured i II Identity (t <:.> u), Extractable_ t, Covariant (->) (->) u, Extractable_ u) => Status
	plane = extract . lower . extract $ view # sub @i # z

	slant :: forall v h . (Substructured v II Identity Vertically, Substructured h (Zipper Stream (Left ::: Right)) Identity Stream) => Status
	slant = extract . extract . view (sub @h) . lower . extract $ view # sub @v # z

conway :: Around -> Status
conway (focused :*: neighbors) = alive == one + one ? focused
	$ alive == one + one + one ? True $ False where

	alive :: Int
	alive = let count status acc = status ? acc + one $ acc in
		reduce count zero neighbors

lifecycle :: (II Status -> Status) -> II Status -> IO ()
lifecycle act being = delay *>- purge *>- snapshot *>- evolve where

	evolve, snapshot :: IO ()
	evolve = lifecycle act $ act <<= being 
	snapshot = void $ let screen = display 5
		in print <<- screen (screen -<$>- run being) 

--------------------------------------------------------------------------------

cube :: II Status
cube = TU . Tap one $ twosome only only where

	only :: Stream :. Zipper Stream (Left ::: Right) := Status
	only = Construct one . Identity $ repeat noone

	one, noone :: Zipper Stream (Left ::: Right) Status
	one = Tap True $ twosome alone alone
	noone = Tap False $ twosome # repeat False # repeat False

	alone :: Stream Status
	alone = Construct True . Identity $ repeat False

blinker :: II Status
blinker = TU . Tap one $ twosome # repeat noone # repeat noone where

	one, noone :: Zipper Stream (Left ::: Right) Status
	one = Tap True $ twosome alone alone
	noone = Tap False $ twosome # repeat False # repeat False

	alone :: Stream Status
	alone = Construct True . Identity $ repeat False

main = lifecycle # conway . around # cube
