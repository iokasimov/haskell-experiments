module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
import "base" System.IO (print)
--
data R (o :: Letter) (f :: Letter) (t :: Letter)
	= R1 (Proxy o)
	| R2 (Proxy o) (Proxy o)
	| R3 (Proxy o) (Proxy o) (Proxy o)
	| R4 (Proxy o) (Proxy f)
	| R5 (Proxy f)
	| R6 (Proxy f) (Proxy o)
	| R7 (Proxy f) (Proxy o) (Proxy o)
	| R8 (Proxy f) (Proxy o) (Proxy o) (Proxy o)
	| R9 (Proxy o) (Proxy t)
	| R10 (Proxy t)

type family Decimal (o :: Letter) (f :: Letter) (t :: Letter) a where
	Decimal I V X a = ()
	Decimal X L C (R I V X) = ()
	Decimal C D M (R X L C) = ()

-- class Numberify a where
-- 	numberify :: a -> Int
--
-- instance Numberify Symbol where
-- 	numberify I = 1
-- 	numberify V = 5
-- 	numberify X = 10
-- 	numberify L = 50
-- 	numberify C = 100
-- 	numberify D = 500
-- 	numberify M = 1000

type family Unit (l :: Letter) a where
	Unit I a = a
	Unit V a = a :*: a :*: a :*: a :*: a
	Unit X a = a :*: a :*: a :*: a :*: a :*: a :*: a :*: a :*: a :*: a

infixr 1 #=

type family (#=) (l :: Letter) b where
	I #= a = a
	V #= a = a :*: a :*: a :*: a :*: a
	X #= a = a :*: a :*: a :*: a :*: a :*: a :*: a :*: a :*: a :*: a

type family (=#) (l :: Letter) b where
	I =# (a :*: r) = r
	V =# (a :*: a :*: a :*: a :*: a :*: r) = r
	X =# (a :*: a :*: a :*: a :*: a :*: a :*: a :*: a :*: a :*: a :*: r) = r

-- v5_to_list :: forall t a . Monotonic a (t a) => t a -> List a
-- v5_to_list = reduce @a @(t a) (item @Push @List) empty

-- example :: V (V Int)
-- example =
-- 	(0 :*: 0 :*: 0 :*: 0 :*: 0) :*:
-- 	(0 :*: 0 :*: 0 :*: 0 :*: 0) :*:
-- 	(0 :*: 0 :*: 0 :*: 0 :*: 0) :*:
-- 	(0 :*: 0 :*: 0 :*: 0 :*: 0) :*:
-- 	(0 :*: 0 :*: 0 :*: 0 :*: 0)

vi :: V #= (I #= Int)
vi = 0 :*: 0 :*: 0 :*: 0 :*: 0

iv :: I =# (V #= Int)
iv = 0 :*: 0 :*: 0 :*: 0

example :: R I V X
example = R5 (Proxy :: Proxy V)

main = void $ do
	print "typechecked"
	-- print . v5_to_list $ (True :*: True :*: True :*: False :*: False :: V Boolean) 
