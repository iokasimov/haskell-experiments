module Main where

import "base" Control.Concurrent (threadDelay)
import "base" Data.Functor.Identity (Identity (Identity, runIdentity))
import "comonad" Control.Comonad (Comonad (extract, duplicate), (=>>))
import "free" Control.Comonad.Cofree (Cofree ((:<)), coiter, unwrap)
import "joint" Control.Joint (type (:.), type (:=), type (~>), TU (TU), run, (<$$>))

type Stream = Cofree Identity

data Z1 a = Z1 (Stream a) a (Stream a)

instance Functor Z1 where
	fmap f (Z1 ls x rs) = Z1 (f <$> ls) (f x) (f <$> rs)

instance Comonad Z1 where
	extract (Z1 _ x _) = x
	duplicate z = Z1 (go left z) z (go right z)

-- TODO: return comonad transormer here?
go :: (Z1 ~> Z1) -> Z1 a -> Stream (Z1 a)
go move = runIdentity . unwrap . coiter (Identity . move)

left :: Z1 ~> Z1
left (Z1 ls x rs) = Z1 (x :< Identity ls) (extract rs) (runIdentity $ unwrap rs)

right :: Z1 ~> Z1
right (Z1 ls x rs) = Z1 (runIdentity $ unwrap ls) (extract ls) (x :< Identity rs)

--------------------------------------------------------------------------------

data Status = Dead | Alive

instance Show Status where
	show Dead = " "
	show Alive = "*"

type Field = Z1 Status

type Neighbours = (Status, Status, Status)

neighbourhood :: Field -> Neighbours
neighbourhood z = (extract $ left z, extract z, extract $ right z)

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

display_1d :: Z1 ~> []
display_1d (Z1 ls x rs) = reverse (take 30 $ listify ls) <> [x] <> take 30 (listify rs)

start_d1 :: Field
start_d1 = let desert = coiter Identity Dead
	in Z1 desert Alive desert

lifecycle :: Field -> (Field -> Status) -> IO ()
lifecycle being act = do
	threadDelay 1000000
	print $ display_1d being
	lifecycle (being =>> act) act

--------------------------------------------------------------------------------

type Z2 = TU Z1 Z1

instance Functor Z2 where
	fmap f (TU zz) = TU $ f <$$> zz

instance Comonad Z2 where
	extract (TU zz) = extract $ extract zz
	duplicate zz = TU $ plane <$> plane zz where

		plane :: Z2 a -> Z1 (Z2 a)
		plane z = TU <$> divergence (run z)

		divergence :: Z1 a -> Z1 (Z1 a)
		divergence x = Z1 (go left x) x (go right x)

display_2d :: Z2 Status -> [[Status]]
display_2d (TU zz) = display_1d $ display_1d <$> zz

start_d2 :: Z2 Status
-- start_d2 = TU $ Z1 (only :< Identity (coiter Identity noone)) only (only :< Identity (coiter Identity noone)) where
start_d2 = TU $ Z1 (coiter Identity noone) inline (coiter Identity noone) where

	only, noone :: Z1 Status
	only = Z1 (coiter Identity Dead) Alive (coiter Identity Dead)
	noone = Z1 (coiter Identity Dead) Dead (coiter Identity Dead)

	inline = Z1 (Alive :< Identity (coiter Identity Dead)) Alive (Alive :< Identity (coiter Identity Dead))

-- (horizontal, vertical, major diagonal, minor diagonal)
type Around = ((Status, Status), (Status, Status), (Status, Status), (Status, Status))

around :: Z2 Status -> Around
around z = (horizontal, vertical, major_diagonal, minor_diagonal) where

	horizontal = (extract . extract . left $ run z, extract . extract . right $ run z)
	vertical = (extract . left . extract $ run z, extract . right . extract $ run z)
	major_diagonal = (extract . left . extract . left $ run z, extract . right . extract . right $ run z)
	minor_diagonal = (extract . left . extract . right $ run z, extract . right . extract . left $ run z)

liveness :: Around -> Int
liveness ((hl, hr), (vl, vr), (mjrl, mjrr), (mnrl, mnrr)) = count where

	count :: Int
	count = intify hl + intify hr + intify vl + intify vr
		+ intify mjrl + intify mjrr + intify mnrl + intify mnrr

	intify :: Status -> Int
	intify Alive = 1
	intify Dead = 0

liferule :: Z2 Status -> Status
liferule z = case liveness $ around z of
	2 -> extract z
	3 -> Alive
	_ -> Dead

conway :: Z2 Status -> (Z2 Status -> Status) -> IO ()
conway field act = do
	threadDelay 1000000
	putStr "\ESC[2J"
	traverse print $ display_2d field
	conway (field =>> act) act


main = do
	conway start_d2 liferule
	-- traverse print $ display_2d start_d2
	-- print $ around start_d2
-- lifecycle start_d1 $ rule90 . neighbourhood
