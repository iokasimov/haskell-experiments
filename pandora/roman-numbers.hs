module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Int, print)

data Symbol = I | V | X | L | C | D | M

type Place (d :: Symbol) = Tagged d ()

data Number (o :: Symbol) (f :: Symbol) (t :: Symbol)
	= D1 (Place o)
	| D2 (Place o) (Place o)
	| D3 (Place o) (Place o) (Place o)
	| D4 (Place o) (Place f)
	| D5 (Place f)
	| D6 (Place f) (Place o)
	| D7 (Place f) (Place o) (Place o)
	| D8 (Place f) (Place o) (Place o) (Place o)
	| D9 (Place o) (Place t)

data family Decimal (o :: Symbol) (f :: Symbol) (t :: Symbol)
data instance Decimal I V X = Unit
data instance Decimal X L C = Ten
data instance Decimal C D M = Hundred

class Numberify a where
	numberify :: a -> Int

instance Numberify Symbol where
	numberify I = 1
	numberify V = 5
	numberify X = 10
	numberify L = 50
	numberify C = 100
	numberify D = 500
	numberify M = 1000

main = print "typechecked"
