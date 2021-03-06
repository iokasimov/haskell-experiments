module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, Integer, print)

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

data Value = Null
	| Bool Boolean
	| String (List Char)
	| Number (Nonempty List Digit) (List Digit) (List Digit)
	| Array (List Value)
	| Object (List (List Char :*: Value))

main = print "typechecked"
