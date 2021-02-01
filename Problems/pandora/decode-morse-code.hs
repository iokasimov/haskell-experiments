import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, print)

both :: a -> Nonempty Binary a -> Nonempty Binary a -> Nonempty Binary a
both x l r = Construct x $ Both l r

(>-) :: a -> Nonempty Binary a -> Nonempty Binary a
x >- r = Construct x $ Right r

(-<) :: Nonempty Binary a -> a -> Nonempty Binary a
l -< x = Construct x $ Left l

leaf :: a -> Nonempty Binary a
leaf x = Construct x End

dictionary :: Dictionary
dictionary = both ' '
	(both 'E'
		(both 'I'
			(both 'S'
				(both 'H'
					(leaf '5')
					(leaf '4')
				)
				(both 'V'
					(leaf ' ')
					(leaf '3')
				)
			)
			(both 'U'
				(leaf 'F')
				(' ' >- leaf '2'
				)
			)
		)
		(both 'A'
			(both 'R'
				(leaf 'L')
				(leaf '+' -< ' ')
			)
			(both 'W'
				(leaf 'P')
				('J' >- leaf '1')
			)
		)
	)
	(both 'T'
		(both 'N'
			(both 'D'
				(both 'B'
					(leaf '6')
					(leaf '=')
				)
				(leaf '/' -< 'X')
			)
			(both 'K'
				(leaf 'C')
				(leaf 'Y')
			)
		)
		(both 'M'
			(both 'G'
				(leaf '7' -< 'Z')
				(leaf 'Q')
			)
			(both 'O'
				(leaf '8' -< ' ')
				(both ' '
					(leaf '9')
					(leaf '0')
				)
			)
		)
	)

data Morse = Dot | Dash

type Dictionary = Nonempty Binary Char

search :: Stack Morse -> Maybe Char
search code = extract . attached <$> run @(State Dictionary :> Maybe) (code ->> decode) dictionary where

	decode :: (Monad t, Optional t, Stateful Dictionary t) => Morse -> t ()
	decode x = current @Dictionary >>= cut x >>= replace

	cut :: (Pointable t, Optional t) => Morse -> Dictionary -> t Dictionary
	cut Dot = adapt . run . view (sub @Left)
	cut Dash = adapt . run . view (sub @Right)

--------------------------------------------------------------------------------

digit4 :: Stack Morse
digit4 = insert Dot . insert Dot . insert Dot . insert Dot . insert Dash $ empty

main = search digit4 & resolve @Char print (print "Not found...")
