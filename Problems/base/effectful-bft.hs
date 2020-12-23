module Main where

-- Source: https://doisinkidney.com/posts/2020-11-23-applicative-queue.html#ref-easterly_functions_2019

import Control.Applicative

data Free f a where
	Pure :: a -> Free f a
	Lift :: (a -> b -> c) -> f a -> Free f b -> Free f c

-- instance Functor f => Functor (Free f) where
-- 	fmap f (Pure x) = Pure $ f x
-- 	fmap f (Lift g x free) = (g <$> x :: _)

-- f :: c -> d
-- g :: a -> b -> c
-- x :: f a
-- g <$> x :: f (b -> c)
-- free :: Free f b

-- instance Applicative f => Applicative (Free f) where
-- 	pure = Pure
--
-- 	liftA2 c (Pure x) ys = fmap (c x) ys
-- 	liftA2 c xs (Pure y) = fmap (flip c y) xs
-- 	liftA2 c (Lift f x xs) (Lift g y ys) =
-- 		Lift
-- 		(\(x,y) (xs,ys) -> c (f x xs) (g y ys))
-- 		(liftA2 (,) x y)
-- 		(liftA2 (,) xs ys)

lower :: Applicative f => Free f a -> f a
lower (Pure x) = pure x
lower (Lift f x xs) = f <$> x <*> lower xs

main = print "typechecked"
