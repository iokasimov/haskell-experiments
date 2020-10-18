module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, Integer, print)

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

data Value = Null
	| Bool Boolean
	| String (Stack Char)
	| Number (Nonempty Stack Digit) (Stack Digit) (Stack Digit)
	| Array (Stack Value)
	| Object (Stack (Stack Char :*: Value))

main = print "typechecked"
