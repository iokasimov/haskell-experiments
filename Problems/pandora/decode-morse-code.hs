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

type Decoder = Nonempty Binary Char

dictionary :: Decoder
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

(.-) Dot x _ = x
(.-) Dash _ y = y

search :: List Morse -> Maybe Char
search code = extract . attached <$> run @(State Decoder :> Maybe) (code ->> decode) dictionary

decode :: (Monad t, Optional t, Stateful Decoder t) => Morse -> t Decoder
decode x = zoom @Decoder (x .- sub @Left $ sub @Right) current
	>>= adapt . run @Binary >>= replace @Decoder

--------------------------------------------------------------------------------

digit4 :: List Morse
digit4 = Dot += Dot += Dot += Dot += Dash += empty

main = search digit4 & resolve @Char print (print "Not found...")
