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

type Directory = Nonempty List Char

type Path = List Directory

data Link = Forward Directory | Previous | Current

settle :: Link -> State Path ()
settle (Forward dir) = void $ modify @Path $ item @Push dir
settle Previous = void $ focus @Head @List @Directory =<> Nothing
settle Current = point ()

solution :: List Link  -> Path
solution unresolved = attached $ run (unresolved ->> settle) empty

--------------------------------------------------------------------------------

usr, bin, scripts :: Directory
usr = item @Push 'u' $ item @Push 's' $ point 'r'
bin = item @Push 'b' $ item @Push 'i' $ point 'n'
scripts = item @Push 's' $ item @Push 'c' $ item @Push 'r' $ item @Push 'i' $ item @Push 'p' $ item @Push 't' $ point 's'

-- /usr/bin/../bin/./scripts/../
example :: List Link
example = item @Push (Forward usr)
	$ item @Push (Forward bin)
	$ item @Push Previous
	$ item @Push (Forward bin)
	$ item @Push Current
	$ item @Push (Forward scripts)
	$ item @Push Previous
	$ empty

main = void $ Reverse (solution example) ->> print
