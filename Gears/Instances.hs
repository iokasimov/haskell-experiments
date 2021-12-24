{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gears.Instances where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import qualified GHC.Int as Base -- (eqInt)
import qualified Prelude as Base -- (Int, Semigroup ((<>)), Show (show), min, max, (+), (-), (*))

deriving instance Base.Show Bracket
deriving instance Base.Show Sign
deriving instance Base.Show Quote
deriving instance Base.Show Slash
deriving instance Base.Show Position

instance Covariant (->) (->) [] where
	f <-|- [] = []
	f <-|- (x : xs) = (f x) : (f <-|- xs)

instance Traversable (->) (->) [] where
	_ <<- [] = point []
	f <<- (x : xs) = (:) <-|- f x <-*- f <<- xs

deriving instance (Base.Show a, Base.Show b) => Base.Show (a :*: b)
deriving instance Base.Show a => Base.Show (Maybe a)
deriving instance (Base.Show e, Base.Show a) => Base.Show (Conclusion e a)
-- deriving instance Base.Show a => Base.Show (Delta a)
deriving instance Base.Show a => Base.Show (Wye a)

deriving instance Base.Show Numerator
deriving instance Base.Show Denumerator

instance Base.Show Boolean where
	show True = "*"
	show False = " "

instance Base.Show a => Base.Show (Construction Maybe a) where
	show (Construct x (Just xs)) = Base.show x Base.++ ", " Base.++ Base.show xs
	show (Construct x Nothing) = Base.show x Base.++ "]"

instance Base.Show a => Base.Show (List a) where
	show (TT (Just stack)) = "[" Base.++ Base.show stack
	show (TT Nothing) = "..."

instance Base.Show a => Base.Show ((Identity <:.:> (List <:.:> List := (:*:)) := (:*:)) := a) where
	show (T_U (Identity x :*: T_U (bs :*: fs))) = "| " Base.<> Base.show bs Base.<> " =: " Base.<> Base.show x Base.<> " := " Base.<> Base.show fs Base.<> " |"

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
