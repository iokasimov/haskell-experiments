import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, print)

dictionary :: Nonempty Binary Char
dictionary = Construct ' ' $ Both
	(Construct 'E' $ Both
		(Construct 'I' $ Both
			(Construct 'S' $ Both
				(Construct 'H' $ Both
					(Construct '5' $ End)
					(Construct '4' $ End)
				)
				(Construct 'V' $ Both
					(Construct ' ' $ End)
					(Construct '3' $ End)
				)
			)
			(Construct 'U' $ Both
				(Construct 'F' $ End)
				(Construct ' ' $ Right
					(Construct '2' $ End)
				)
			)
		)
		(Construct 'A' $ Both
			(Construct 'R' $ Both
				(Construct 'L' $ End)
				(Construct ' ' $ Left
					(Construct '+' $ End)
				)
			)
			(Construct 'W' $ Both
				(Construct 'P' $ End)
				(Construct 'J' $ Right
					(Construct '1' $ End)
				)
			)
		)
	)
	(Construct 'T' $ Both
		(Construct 'N' $ Both
			(Construct 'D' $ Both
				(Construct 'B' $ Both
					(Construct '6' $ End)
					(Construct '=' $ End)
				)
				(Construct 'X' $ Left
					(Construct '/' $ End)
				)
			)
			(Construct 'K' $ Both (Construct 'C' $ End) (Construct 'Y' $ End))
		)
		(Construct 'M' $ Both
			(Construct 'G' $ Both
				(Construct 'Z' $ Left
					(Construct '7' $ End)
				)
				(Construct 'Q' $ End))
			(Construct 'O' $ Both
				(Construct ' ' $ Left (Construct '8' $ End))
				(Construct ' ' $ Both (Construct '9' $ End) (Construct '0' $ End))
			)
		)
	)

data Morse = Dot | Dash

type Dictionary = Nonempty Binary Char

cut :: (Pointable t, Optional t) => Morse -> Dictionary -> t Dictionary
cut Dot = adapt . extract . view (sub @Left)
cut Dash = adapt . extract . view (sub @Right)

decode :: (Monad t, Optional t, Stateful Dictionary t) => Morse -> t ()
decode x = current @Dictionary >>= cut x >>= replace

digit4 :: Stack Morse
digit4 = push Dot . push Dot . push Dot . push Dot . push Dash $ empty

main = maybe (print "Not found...") (print . extract . attached)
	. run @(State Dictionary :> Maybe) % dictionary $ digit4 ->> decode
