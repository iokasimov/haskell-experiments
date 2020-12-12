module Main where

import "base" Control.Applicative (Alternative (empty, (<|>)))
import "base" Data.Functor.Identity (Identity)
import "comonad" Control.Comonad (Comonad (extract, duplicate, extend))
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "joint" Control.Joint (Adaptable (adapt), Stateful, State, current, modify, run, type (:>), type (:=))

type Status = Bool

type Stream = Cofree Identity

data Zipper a = Zipper (Stream a) a (Stream a)

instance Functor Zipper where
	fmap f (Zipper ls x rs) = Zipper (f <$> ls) (f x) (f <$> rs)

instance Comonad Zipper where
	extract (Zipper _ x _) = x
	duplicate (Zipper ls x rs) = _

type Field = Zipper Status

main = print "typechecked"
