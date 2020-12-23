module Main where

--------------------------------------------------------------------------------

-- class Constructable e a where
-- 	construct :: e -> a -> [e]
--
-- instance Constructable Int () where
-- 	construct x _ = [x]
--
-- instance Constructable Int (Int -> Int) where
-- 	-- construct x = \y -> x : construct y ()
-- 	construct x f = x : construct (f x)

--------------------------------------------------------------------------------

class Listify e r where
	list :: r -> [e]

instance Listify e [e] where
	list l = l

-- instance Listify e r => Listify e (e -> r) where
-- 	list f = x : list r

main = print "typechecked"
