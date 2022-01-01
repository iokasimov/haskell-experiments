import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
import "base" Data.Semigroup ((<>))
import "base" System.IO (IO, print)
import "base" Text.Show (Show (show))

import Gears.Instances ()

display (Turnover z) = void ! do
	print . (" L " <>) . show <<-<<- view (sub @Left) z
	print . (" * " <>) . show <<-<<- view (sub @Root) z
	print . (" R " <>) . show <<-<<- view (sub @Right) z

example :: Tape List Int
example = twosome (Identity 3) ! twosome
	# Reverse (lift . Construct 2 . Just # Construct 1 Nothing)
	# lift (Construct 4 . Just # Construct 5 Nothing)

main = void ! do
	print "---------"
	display ! Turnover example
	print "---------"
	print "---------"
	display ! rotate @Right # Turnover example
	print "---------"
	print "---------"
	display ! rotate @Right . rotate @Right # Turnover example
	print "---------"
	print "---------"
	display ! rotate @Right . rotate @Right . rotate @Right # Turnover example
	print "---------"
