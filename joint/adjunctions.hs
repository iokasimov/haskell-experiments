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

type Lens s t = s -> Store t s

view :: Lens s t -> s -> t
view lens = pos . lens

set :: Lens s t -> t -> s -> s
set lens new = peek new . lens

over :: Lens s t -> (t -> t) -> s -> s
over lens f = extract . retrofit f . lens

example_lens :: Lens (Bool, Int) Int
example_lens (b, i) = Store (i, (,) b)

try_right_adj = store |- state . fst where

	state :: Bool -> State Int String
	state True = modify @Int (const 0) $> show True
	state False = pure $ show False

	store :: Store Int (Bool, Int)
	store = Store (1, (,) False)

main = print try_right_adj
