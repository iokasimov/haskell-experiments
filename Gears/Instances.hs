{-# LANGUAGE FlexibleInstances #-}

module Gears.Instances where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import qualified GHC.Int as Base -- (eqInt)
import qualified Prelude as Base -- (Int, Semigroup ((<>)), Show (show), min, max, (+), (-), (*))

instance Covariant [] where
	f <$> [] = []
	f <$> (x : xs) = (f x) : (f <$> xs)

instance Traversable [] where
	[] ->> _ = point []
	(x : xs) ->> f = (:) <$> f x <*> (xs ->> f)

deriving instance (Base.Show a, Base.Show b) => Base.Show (a :*: b)
deriving instance Base.Show a => Base.Show (Maybe a)
-- deriving instance Base.Show a => Base.Show (Delta a)
deriving instance Base.Show a => Base.Show (Wye a)

deriving instance Base.Show Numerator
deriving instance Base.Show Denumerator

instance Base.Show Boolean where
	show True = "*"
	show False = " "

instance Base.Show a => Base.Show (Construction Maybe a) where
	show (Construct x (Just xs)) = Base.show x Base.++ ", " Base.++ Base.show xs
	show (Construct x Nothing) = Base.show x

instance Base.Show a => Base.Show (Stack a) where
	show (TU (Just stack)) = Base.show stack
	show (TU Nothing) = ""

-- instance Base.Show Numerator where
-- 	show Zero = "0"
-- 	show (Numerator n) = "1" Base.<> Base.show n

instance Semigroup Base.Int where
	(+) = (Base.+)

instance Monoid Base.Int where
	zero = 0

instance Ringoid Base.Int where
	(*) = (Base.*)

instance Quasiring Base.Int where
	one = 1

instance Group Base.Int where
	invert = Base.negate

instance Infimum Base.Int where
	(/\) = Base.min

instance Supremum Base.Int where
	(\/) = Base.max

instance Setoid Base.Int where
	x == y = if Base.eqInt x y then True else False

instance Chain Base.Int where
	x <=> y = case Base.compare x y of
		Base.GT -> Greater
		Base.EQ -> Equal
		Base.LT -> Less

instance Setoid Base.Char where
	x == y = if x Base.== y then True else False
