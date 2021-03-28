module Main where

data Dijkstra s a = Dijkstra { wp :: forall r . ((s, a) -> r) -> s -> r }

instance Functor (Dijkstra s) where
	fmap f (Dijkstra d) = Dijkstra $ d . (. (f <$>))

instance Applicative (Dijkstra s) where
	pure x = Dijkstra $ \f s -> f (s, x)
	Dijkstra f <*> Dijkstra x = undefined

instance Monad (Dijkstra s) where
	return x = Dijkstra $ \f s -> f (s, x)
	m >>= f = Dijkstra $ \g -> wp m (\(s, a) -> wp (f a) g s)

main = print "typechecked"
