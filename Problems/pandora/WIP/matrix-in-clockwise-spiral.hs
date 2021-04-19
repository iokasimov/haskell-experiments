import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Int, print)

import Gears.Instances ()

example :: List Int
example = into @List . vectorize @Int
	$ (1 :: Int) :*: (2 :: Int) :*: (3 :: Int)

main = print example
