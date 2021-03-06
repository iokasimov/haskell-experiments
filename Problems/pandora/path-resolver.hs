module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO
import "base" Data.Char (Char)
import "base" Data.Int (Int)
import "base" Data.Semigroup ((<>))
import "base" System.IO (print)
import "base" Text.Show (Show (show))

import Gears.Instances ()

newtype Directory = Directory (Nonempty Stack Char)

instance Show Directory where
	show (Directory (Construct x (Just xs))) = x : show (Directory xs)
	show (Directory (Construct x Nothing)) = x : "/"

data Part = Forward Directory | Previous | Current

instance Show Part where
	show (Forward dir) = show dir
	show Previous = "../"
	show Current = "./"

type Path = Stack Part

instance {-# OVERLAPS #-} Show Path where
	show (TU (Just (Construct x (Just xs)))) = show x <> show (lift xs :: Path)
	show (TU (Just (Construct x Nothing))) = show x
	show (TU Nothing) = ""

settle :: Part -> State (Stack Directory) ()
settle (Forward dir) = void $ modify @(Stack Directory) (dir +=)
settle Previous = void $ focus @Head @Stack @Directory =<> Nothing
settle Current = point ()

solution :: Stack Directory
solution = attached $ run (example ->> settle) empty

--------------------------------------------------------------------------------

usr, bin, scripts :: Directory
usr = Directory $ 'u' += 's' += point 'r'
bin = Directory $ 'b' += 'i' += point 'n'
scripts = Directory $ 's' += 'c' += 'r' += 'i' += 'p' += 't' += point 's'

-- /usr/bin/../bin/./scripts/../
example :: Stack Part
example = Forward usr += Forward bin += Previous += Forward bin
	+= Current += Forward scripts += Previous += empty

main = void $ Reverse solution ->> print
