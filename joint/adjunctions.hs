module Main where

import "comonad" Control.Comonad
import "joint" Control.Joint

infixl 4 -|, |-

class (Functor t, Functor u) => Adjoint t u where
	{-# MINIMAL (-|), (|-) #-}
	-- | Left adjunction
	(-|) :: a -> (t a -> b) -> u b
	-- | Right adjunction
	(|-) :: t a -> (a -> u b) -> b

newtype Store s a = Store ((,) s :. (->) s := a)

instance Functor (Store s) where
	fmap f (Store x) = Store $ f <$$> x

instance Comonad (Store s) where
	extract (Store (s, f)) = f s
	extend g (Store (s, f)) = Store (s, g . (\p -> Store (p, f)))

instance Adjoint (Store s) (State s) where
	(-|) :: a -> (Store s a -> b) -> State s b
	x -| f = State $ \s -> (,) s . f $ Store (s, const x)
	(|-) :: Store s a -> (a -> State s b) -> b
	Store (s, f) |- g = extract . flip run s . g $ f s

main = print "typechecked"
