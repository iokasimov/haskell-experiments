module Main where

-- Source: paper "Understanding Idiomatic Traversals Backwards and Forwards"

import Data.Functor

data Batch e r a = Barely a | Plan e (Batch e r (r -> a))

instance Functor (Batch e r) where
	fmap f (Barely c) = Barely $ f c
	fmap f (Plan a u) = Plan a $ (f .) <$> u

instance Applicative (Batch e r) where
	pure = Barely
	Barely f <*> Barely x = Barely $ f x
	Plan a u <*> Barely x = Plan a $ Barely (($ x) .) <*> u
	u <*> Plan a v = Plan a $ Barely (.) <*> u <*> v

batch :: e -> Batch e r r
batch x = Plan x $ Barely id

runWith :: Applicative t => (e -> t r) -> Batch e r a -> t a
runWith f (Barely x) = pure x
runWith f (Plan x u) = runWith f u <*> f x

--------------------------------------------------------------------------------

data Bin a = Tip a | Bin (Bin a) (Bin a)

instance Functor Bin where
	fmap f (Tip x) = Tip $ f x
	fmap f (Bin l r) = Bin (f <$> l) (f <$> r)

instance Foldable Bin where
	foldr f acc (Tip x) = f x acc
	foldr f acc (Bin l r) = foldr f (foldr f acc l) r

instance Traversable Bin where
	traverse f (Tip x) = pure Tip <*> f x
	traverse f (Bin u v) = Bin <$> traverse f u <*> traverse f v

example = Bin (Bin (Tip 1) (Tip 2)) (Tip 3)

--------------------------------------------------------------------------------

data Sort x y a where
	Pure :: a -> Sort x y a
	Sort :: x -> Sort x y (y -> a) -> Sort x y a

instance Show x => Show (Sort x y a) where
	show (Pure _) = "Pure"
	show (Sort o f) = "Sort (" ++ show o ++ ") (" ++ show f ++ ")"

instance Functor (Sort x y) where
	fmap f (Pure x) = Pure $ f x
	fmap f (Sort a b) = Sort a $ (f .) <$> b

instance Ord x => Applicative (Sort x y) where
	pure = Pure
	Pure f <*> a = f <$> a
	f <*> Pure a = ($ a) <$> f
	Sort a f <*> Sort b g = if a < b
		then Sort a $ flip <$> f <*> Sort b g
		else Sort b $ flip <$> f <*> Sort a g
	 	-- else Sort b $ flip <$> f <*> Sort a g

-- f :: Sort x y (y -> a -> b)
-- g :: Sort x y (y -> a)

liftSort :: x -> Sort x y y
liftSort a = Sort a $ Pure id

runSort :: (x -> y) -> Sort x y a -> a
runSort _ (Pure a) = a
runSort f (Sort a g) = runSort f g (f a)

sortTraversable :: (Ord a, Traversable t) => t a -> t a
sortTraversable = runSort id . traverse liftSort

--------------------------------------------------------------------------------

-- main = runWith print . traverse batch $ example

-- main = print $ sortTraversable [1,3,4,2,6,9,13,8]
main = do
	print $ sortTraversable [1,3,4,2,6,9,13,12,8]
	print $ sortTraversable [10,9,8,7,6,5,4,3,2,1]
