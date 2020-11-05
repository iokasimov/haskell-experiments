import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, print)

start :: Zipper Stack Boolean
start = let desert = linearize $ repeat False
	in Tap False . TU $ desert :^: desert

main = print "typechecked"
