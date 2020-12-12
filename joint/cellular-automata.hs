module Main where

import "base" Control.Concurrent (threadDelay)
import "base" Data.Functor.Identity (Identity (Identity, runIdentity))
import "comonad" Control.Comonad (Comonad (extract, duplicate), (=>>))
import "free" Control.Comonad.Cofree (Cofree ((:<)), coiter, unwrap)
import "joint" Control.Joint (type (:.), type (:=), type (~>))

type Stream = Cofree Identity

data Zipper a = Zipper (Stream a) a (Stream a)

instance Functor Zipper where
	fmap f (Zipper ls x rs) = Zipper (f <$> ls) (f x) (f <$> rs)

instance Comonad Zipper where
	extract (Zipper _ x _) = x
	duplicate z = Zipper (go left z) z (go right z)

-- TODO: return comonad transormer here?
go :: (Zipper ~> Zipper) -> Zipper a -> Stream (Zipper a)
go move = runIdentity . unwrap . coiter (Identity . move)

left :: Zipper ~> Zipper
left (Zipper ls x rs) = Zipper (x :< Identity ls) (extract rs) (runIdentity $ unwrap rs)

right :: Zipper ~> Zipper
right (Zipper ls x rs) = Zipper (runIdentity $ unwrap ls) (extract ls) (x :< Identity rs)

--------------------------------------------------------------------------------

data Status = Dead | Alive

instance Show Status where
	show Dead = " "
	show Alive = "*"

type Field = Zipper Status

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

display :: Zipper ~> []
display (Zipper ls x rs) = reverse (take 30 $ listify ls) <> [x] <> take 30 (listify rs)

start :: Field
start = let desert = coiter Identity Dead
	in Zipper desert Alive desert

lifecycle :: Field -> (Field -> Status) -> IO ()
lifecycle being act = do
	threadDelay 1000000
	print $ display being
	lifecycle (being =>> act) act

main = lifecycle start $ rule90 . neighbourhood
