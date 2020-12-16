module Main where

import "base" Control.Concurrent (threadDelay)
import "base" Debug.Trace (traceShow)
import "base" Data.Functor.Identity (Identity (Identity, runIdentity))
import "comonad" Control.Comonad (Comonad (extract, duplicate), (=>>))
import "free" Control.Comonad.Cofree (Cofree ((:<)), coiter, unwrap)
import "joint" Control.Joint (type (:.), type (:=), type (~>), TU (TU), run, (<$$>))

type Stream = Cofree Identity

data Zipper a = Zipper (Stream a) a (Stream a)

instance Functor Zipper where
	fmap f (Zipper ls x rs) = Zipper (f <$> ls) (f x) (f <$> rs)

instance Comonad Zipper where
	extract (Zipper _ x _) = x
	duplicate z = Zipper (go backward z) z (go forward z)

go :: (t ~> t) -> t a -> Stream (t a)
go move = runIdentity . unwrap . coiter (Identity . move)

backward :: Zipper ~> Zipper
backward (Zipper ls x rs) = Zipper (runIdentity $ unwrap ls) (extract ls) (x :< Identity rs)

forward :: Zipper ~> Zipper
forward (Zipper ls x rs) = Zipper (x :< Identity ls) (extract rs) (runIdentity $ unwrap rs)

--------------------------------------------------------------------------------

data Status = Dead | Alive

instance Show Status where
	show Dead = " "
	show Alive = "*"

type Field = Zipper Status

type Neighbours = (Status, Status, Status)

neighbourhood :: Field -> Neighbours
neighbourhood z = (extract $ backward z, extract z, extract $ forward z)

rule90 :: Neighbours -> Status
rule90 (Alive, Alive, Alive) = Dead
rule90 (Alive, Alive, Dead) = Alive
rule90 (Alive, Dead, Alive) = Dead
rule90 (Alive, Dead, Dead) = Alive
rule90 (Dead, Alive, Alive) = Alive
rule90 (Dead, Alive, Dead) = Dead
rule90 (Dead, Dead, Alive) = Alive
rule90 (Dead, Dead, Dead) = Dead

listify :: Stream ~> []
listify s = extract s : listify (runIdentity $ unwrap s)

display :: Zipper ~> []
display (Zipper ls x rs) = reverse (take 25 $ listify ls) <> [x] <> take 25 (listify rs)

start_d1 :: Field
start_d1 = let desert = coiter Identity Dead
	in Zipper desert Alive desert

lifecycle :: Field -> (Field -> Status) -> IO ()
lifecycle being act = do
	threadDelay 1000000
	print $ display being
	lifecycle (being =>> act) act

--------------------------------------------------------------------------------

type Grid = TU Zipper Zipper

instance Functor Grid where
	fmap f (TU zz) = TU $ f <$$> zz

instance Comonad Grid where
	extract = extract . extract . run
	duplicate zz = TU $ horizontal <$> vertical zz where

		vertical, horizontal :: Grid a -> Zipper (Grid a)
		vertical x = Zipper (go down x) x (go up x)
		horizontal x = Zipper (go left x) x (go right x)

		up, down, left, right :: Grid ~> Grid
		up = TU . forward . run
		down = TU . backward . run
		left = TU . (backward <$>) . run
		right = TU . (forward <$>) . run

blinker :: Grid Status
blinker = TU $ Zipper (only :< Identity (coiter Identity noone)) only (only :< Identity (coiter Identity noone)) where

glider :: Grid Status
glider = TU $ Zipper (coiter Identity noone) noone shape where

	s1, s2, s3 :: Stream Status
	s1 = Dead :< Identity (Alive :< Identity (coiter Identity Dead))
	s2 = Dead :< Identity (Dead :< Identity (Alive :< Identity (coiter Identity Dead)))
	s3 = Alive :< Identity (Alive :< Identity (Alive :< Identity (coiter Identity Dead)))

	line :: Stream Status -> Zipper Status
	line detail = Zipper (coiter Identity Dead) Dead detail -- (detail :< Identity (coiter Identity Dead))

	shape :: Stream :. Zipper := Status
	shape = line s1 :< Identity (line s2 :< Identity (line s3 :< Identity (coiter Identity noone)))

only, noone :: Zipper Status
only = Zipper (coiter Identity Dead) Alive (coiter Identity Dead)
noone = Zipper (coiter Identity Dead) Dead (coiter Identity Dead)

-- (horizontal, vertical, major diagonal, minor diagonal)
type Around = ((Status, Status), (Status, Status), (Status, Status), (Status, Status))

around :: Grid Status -> Around
around z = (horizontal, vertical, minor_diagonal, major_diagonal) where

	horizontal = (extract . extract . backward $ run z, extract . extract . forward $ run z)
	vertical = (extract . backward . extract $ run z, extract . forward . extract $ run z)
	major_diagonal = (extract . backward . extract . backward $ run z, extract . forward . extract . forward $ run z)
	minor_diagonal = (extract . backward . extract . forward $ run z, extract . forward . extract . backward $ run z)

liveness :: Around -> Int
liveness ((hl, hr), (vl, vr), (mjrl, mjrr), (mnrl, mnrr)) = count where

	count :: Int
	count = intify hl + intify hr + intify vl + intify vr
		+ intify mjrl + intify mjrr + intify mnrl + intify mnrr

	intify :: Status -> Int
	intify Alive = 1
	intify Dead = 0

liferule :: Grid Status -> Status
liferule z = case liveness $ around z of
	2 -> extract z
	3 -> Alive
	_ -> Dead

conway :: Grid Status -> (Grid Status -> Status) -> IO ()
conway field act = do
	threadDelay 100000
	putStr "\ESC[2J"
	traverse print $ display <$> display (run field)
	conway (field =>> act) act

main = conway glider liferule
