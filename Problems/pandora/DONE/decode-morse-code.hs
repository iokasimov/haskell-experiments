{-# LANGUAGE AllowAmbiguousTypes #-}

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Char, print)
import Gears.Instances ()

both :: a -> Nonempty Binary a -> Nonempty Binary a -> Nonempty Binary a
both x l r = Construct x $ Both l r

(>-) :: a -> Nonempty Binary a -> Nonempty Binary a
x >- r = Construct x $ Right r

(-<) :: Nonempty Binary a -> a -> Nonempty Binary a
l -< x = Construct x $ Left l

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

search :: List Morse -> Maybe Char
search code = extract . attached -<$>- run @(State Decoder :> Maybe) (decode <<- code :: _) dictionary

decode :: (Bindable t (->), Optional t, Stateful Decoder t) => Morse -> t Decoder
decode Dot = reconcile $ view # sub @Left
decode Dash = reconcile $ view # sub @Right

--------------------------------------------------------------------------------

digit4 :: List Morse
digit4 = item @Push Dot $ item @Push Dot $ item @Push Dot $ item @Push Dot $ item @Push Dash $ TU Nothing

main = search digit4 & resolve @Char print (print "Not found...")
