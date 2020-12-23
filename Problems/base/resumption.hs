import "transformers" Control.Monad.Trans.State

-- Source: Cheap (but functional) threads

-- data Resumption a = Done a | Pause (State Int (Resumption a))
--
-- instance Functor Resumption where
-- 	fmap f (Done x) = Done $ f x
-- 	fmap f (Pause r) = Pause $ f <$> r
--
-- instance Applicative Resumption where
-- 	Done f <*> Done x = Done $ f x
-- 	Done f <*> Pause r = Done f <*> r
-- 	Pause f <*> Done x = f <*> Done x
-- 	Pause f <*> Pause r = f <*> r
-- 	pure = Done
--
-- instance Monad Resumption where
-- 	Done x >>= f = f x
-- 	Pause r >>= f = Pause $ r >>= f

main = print "typechecked"
