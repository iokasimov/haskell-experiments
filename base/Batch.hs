module Main where

-- Source: paper "Understanding Idiomatic Traversals Backwards and Forwards"

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

main = runWith print . traverse batch $ example
