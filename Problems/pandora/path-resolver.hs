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

type Directory = Nonempty Stack Char

type Path = Stack Directory

data Link = Forward Directory | Previous | Current

settle :: Link -> State Path ()
settle (Forward dir) = void $ modify @Path (dir +=)
settle Previous = void $ focus @Head @Stack @Directory =<> Nothing
settle Current = point ()

solution :: Stack Link  -> Path
solution unresolved = attached $ run (unresolved ->> settle) empty

--------------------------------------------------------------------------------

usr, bin, scripts :: Directory
usr = 'u' += 's' += point 'r'
bin = 'b' += 'i' += point 'n'
scripts = 's' += 'c' += 'r' += 'i' += 'p' += 't' += point 's'

-- /usr/bin/../bin/./scripts/../
example :: Stack Link
example = Forward usr += Forward bin += Previous += Forward bin
	+= Current += Forward scripts += Previous += empty

main = void $ Reverse (solution example) ->> print
