module Main where

import "base" Data.Functor (($>))
import "base" Data.Bifunctor (bimap)
import "comonad" Control.Comonad (extract)
import "joint" Control.Joint (State (State), Store (Store), Lens, run, current, modify, zoom)

infixl 4 -|, |-

class (Functor t, Functor u) => Adjoint t u where
	{-# MINIMAL (-|), (|-) #-}
	-- | Left adjunction
	(-|) :: a -> (t a -> b) -> u b
	-- | Right adjunction
	(|-) :: t a -> (a -> u b) -> b

instance Adjoint ((,) r) ((->) r) where
	(-|) :: a -> ((r, a) -> b) -> (r -> b)
	x -| f = \a -> f (a, x)
	(|-) :: (r, a) -> (a -> r -> b) -> b
	(a, x) |- f = f x a

--------------------------------------------------------------------------------

instance Adjoint (Store s) (State s) where
	(-|) :: a -> (Store s a -> b) -> State s b
	x -| f = State $ \s -> (,) s . f $ Store (s, const x)
	(|-) :: Store s a -> (a -> State s b) -> b
	Store (s, f) |- g = extract . flip run s . g $ f s

--------------------------------------------------------------------------------

try_right_adj :: String
try_right_adj = store |- to_state . fst where

	to_state :: Bool -> State Int String
	to_state True = modify @Int (const 0) $> show True
	to_state False = pure $ show False

	store :: Store Int (Bool, Int)
	store = Store (1, (,) False)

try_left_adj :: State Int Int
try_left_adj = (True, 1) -| from_store where

	from_store :: Store Int (Bool, Int) -> Int
	from_store = snd . extract

--------------------------------------------------------------------------------

bg_state :: String -> State (Bool, Int) ()
bg_state s = modify @(Bool, Int) $ bimap
	(const . even $ length s) (const $ length s)

example_zoom :: State (Bool, Int) String
example_zoom = zoom lens ls_state where

	lens :: Lens (Bool, Int) Int
	lens (b, i) = Store (i, (,) b)

	ls_state :: State Int String
	ls_state = current @Int >>= \case
		1 -> pure "Satisfied..."
		_ -> pure "Not satisfied"

main = do
    print $ run (example_zoom >>= bg_state) (True, 0)
    print $ run (example_zoom >>= bg_state) (True, 1)
