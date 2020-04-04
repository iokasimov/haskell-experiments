import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, print)

dictionary :: Nonempty Binary Char
dictionary = Twister ' ' $ Both
	(Twister 'E' $ Both
		(Twister 'I' $ Both
			(Twister 'S' $ Both
				(Twister 'H' $ Both
					(Twister '5' $ End)
					(Twister '4' $ End)
				)
				(Twister 'V' $ Both
					(Twister ' ' $ End)
					(Twister '3' $ End)
				)
			)
			(Twister 'U' $ Both
				(Twister 'F' $ End)
				(Twister ' ' $ Right
					(Twister '2' $ End)
				)
			)
		)
		(Twister 'A' $ Both
			(Twister 'R' $ Both
				(Twister 'L' $ End)
				(Twister ' ' $ Left
					(Twister '+' $ End)
				)
			)
			(Twister 'W' $ Both
				(Twister 'P' $ End)
				(Twister 'J' $ Right
					(Twister '1' $ End)
				)
			)
		)
	)
	(Twister 'T' $ Both
		(Twister 'N' $ Both
			(Twister 'D' $ Both
				(Twister 'B' $ Both
					(Twister '6' $ End)
					(Twister '=' $ End)
				)
				(Twister 'X' $ Left
					(Twister '/' $ End)
				)
			)
			(Twister 'K' $ Both (Twister 'C' $ End) (Twister 'Y' $ End))
		)
		(Twister 'M' $ Both
			(Twister 'G' $ Both
				(Twister 'Z' $ Left
					(Twister '7' $ End)
				)
				(Twister 'Q' $ End))
			(Twister 'O' $ Both
				(Twister ' ' $ Left (Twister '8' $ End))
				(Twister ' ' $ Both (Twister '9' $ End) (Twister '0' $ End))
			)
		)
	)

data Morse = Dot | Dash

type Dictionary = Nonempty Binary Char

cut :: (Pointable t, Optional t) => Morse -> Dictionary -> t Dictionary
cut Dot = adapt . left . untwist
cut Dash = adapt . right . untwist

decode :: (Monad t, Optional t, Stateful Dictionary t) => Morse -> t ()
decode x = current @Dictionary >>= cut x >>= replace

digit4 :: Stack Morse
digit4 = push Dot . push Dot . push Dot . push Dot . push Dash $ empty

main = maybe (print "Not found...") (print . extract . attached)
	. run @(State Dictionary :> Maybe) % dictionary $ digit4 ->> decode
