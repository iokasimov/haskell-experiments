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

instance Adjoint ((,) a) ((->) a) where
	x -| f = \a -> f (a, x)
	(a, x) |- f = f x a

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

pos :: Store s a -> s
pos (Store (s, _)) = s

seek :: s -> Store s a -> Store s a
seek s (Store (_, f)) = Store (s, f)

peek :: s -> Store s a -> a
peek s (Store (_, f)) = f s

retrofit :: (s -> s) -> Store s a -> Store s a
retrofit g (Store (s, f)) = Store (g s, f)

main = print "typechecked"
