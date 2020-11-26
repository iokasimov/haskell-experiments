module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Int, print)

-- data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving Show
--
-- type Loc a = (Tree a, Context a)
--
-- data Context a = Top
--     | L (Tree a) (Context a)
--     | R (Tree a) (Context a)
--     deriving Show
--
-- left :: Loc a -> Loc a
-- left (Branch l r, c) = (l, L r c)
--
-- right :: Loc a -> Loc a
-- right (Branch l r, c) = (r, R l c)
--
-- top :: Tree a -> Loc a
-- top t = (t, Top)
--
-- up :: Loc a -> Loc a
-- up (t, L r c) = (Branch t r, c)
-- up (t, R l c) = (Branch l t, c)
--
-- upmost :: Loc a -> Loc a
-- upmost l@(t, Top) = l
-- upmost l = upmost $ up l
--
-- modify :: Loc a -> (Tree a -> Tree a) -> Loc a
-- modify (t, c) f = (f t, c)
--

data Biforked' a = Top' | Leftward' a | Rightward' a

--------------------------------------------------------------------------------

-- example_tree :: Tree Int
-- example_tree = Branch
--     (Branch (Leaf 1) (Leaf 2))
--     (Branch (Leaf 3) (Leaf 4))

leaf :: a -> Nonempty Binary a
leaf x = Construct x End

example :: Nonempty Binary Int
example = Construct 1 $ Both
	(Construct 2 $ Both (leaf 4) (leaf 5))
	(Construct 3 $ Left (leaf 6))

-- Focus on leaf 4
-- example_zipper :: Construction Biforked (Nonempty Binary Int)
-- example_zipper = Construct (leaf 4) $ Leftward $ Construct (leaf 5) $
-- 	Leftward

-- Focus on whole binary tree
-- example_zipper' :: Construction Biforked (Nonempty Binary Int)
-- example_zipper' = Construct example Top

-- data Biforked' t a = Top'
-- 	| Leftward' (t a) a (Biforked' t a)
-- 	| Rightward' a (t a) (Biforked' t a)

-- Focus on leaf 4
example_zipper :: Construction Wye <:.:> ((Biforked' <:.> Construction Biforked') <:.> T_ Covariant (Maybe <:.> Construction Wye)) := Int
example_zipper = T_U . (:*:) (leaf 4) . TU . TU . Leftward'
	$ Construct (T_ $ 2 :*: TU (Just $ leaf 5)) . Leftward'
	$ Construct (T_ $ 1 :*: TU (Just . Construct 3 . Left $ leaf 6)) $ Top'

up (T_U (x :*: (TU ctx))) = undefined

-- downright :: Construction Wye <:.:> (Construction Biforked' <:.> T_ Covariant (Maybe <:.> Construction Wye)) := a
-- 	-> Construction Wye <:.:> (Construction Biforked' <:.> T_ Covariant (Maybe <:.> Construction Wye)) := a
-- downright (T_U (x :*: ctx)) =
-- downright (Branch l r, c) = (r, R l c)

-- type instance Zipper (Construction Wye) = Construction Biforked' <:.> (T_ Covariant (Maybe <:.> Construction Wye))

-- Focus on left subtree§§§§§§§
-- example_zipper' :: Construction (Biforked' (Construction Wye)) (Nonempty Binary Int)
-- example_zipper' = Construct (Construct 2 $ Both (leaf 4) (leaf 5))
-- 	$ Leftward $ Construct (Construct 3 $ Left (leaf 6))
-- 		$

-- data Jack t a = It a | Other (t a)

-- Focus on left subtree
-- example_zipper'' :: Nonempty Binary Int :*: Biforked' (Construction Wye) Int
-- example_zipper'' = (Construct 2 $ Both (leaf 4) (leaf 5))
-- 	:*: Leftward' (Construct 3 $ Left (leaf 6)) 1 Top'

-- Focus on leaf 4
-- example_zipper''' :: Nonempty Binary Int :*: Biforked' (Construction Wye) Int
-- example_zipper''' = leaf 4 :*: Leftward' (leaf 5) 2
-- 	(Leftward' (Construct 3 $ Left (leaf 6)) 1 Top')

main = print "typechecked"
