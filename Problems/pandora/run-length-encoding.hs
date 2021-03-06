import "base" Data.Char (Char)
import "base" Data.Int (Int)
import "base" System.IO (print)

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances

type Occurence = Int :*: Char

type Counter = List Occurence

proceed :: Char -> State Counter ()
proceed next = zoom @Counter (focus @Head) (current @(Maybe Occurence)) >>= \case
	Just (n :*: previous) -> next == previous
		? void (zoom @Counter (focus @Head) (replace . Just $ n + 1 :*: previous))
		$ void (modify @Counter ((1 :*: next) +=))
	Nothing -> void $ modify @Counter ((1 :*: next) +=)

--------------------------------------------------------------------------------

example :: List Char
example = 'a' += 'a' += 'a' += 'a' += 'b' += 'b' += 'b' += 'c' += 'c' += 'a' += empty

main = void $ do
	print example
	print . attached $ run (Reverse example ->> proceed) empty
