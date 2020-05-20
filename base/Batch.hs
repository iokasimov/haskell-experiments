module Main where

-- Source: paper "Understanding Idiomatic Traversals Backwards and Forwards"

data Batch a b c = P c | Batch a b (b -> c) :*: a

instance Functor (Batch a b) where
	fmap f (P c) = P $ f c
	fmap f (u :*: a) = fmap (f .) u :*: a

instance Applicative (Batch a b) where
	pure = P
	P f <*> P x = P $ f x
	u :*: a <*> P x = (P (($ x) .) <*> u) :*: a
	u <*> (v :*: a) = (P (.) <*> u <*> v) :*: a

batch :: a -> Batch a b b
batch x = P id :*: x

runWith :: Applicative m => (a -> m b) -> Batch a b c -> m c
runWith f (P x) = pure x
runWith f (u :*: x) = runWith f u <*> f x

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

main = runWith print . traverse batch $ example
