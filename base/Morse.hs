module Main where

-- Source: https://apfelmus.nfshost.com/articles/fun-with-morse-code.html

import Prelude hiding (Either (Left, Right))
import Control.Comonad.Cofree (Cofree ((:<)))

data Wye a = End | Left a | Right a | Both a a

instance Functor Wye where
	fmap _ End = End
	fmap f (Left x) = Left $ f x
	fmap f (Right y) = Right $ f y
	fmap f (Both x y) = Both (f x) (f y)

-- Non-empty binary tree
type Binary = Cofree Wye

dictionary :: Binary Char
dictionary = ' ' :< Both
	('E' :< Both
		('I' :< Both
			('S' :< Both
				('H' :< Both
					('5' :< End)
					('4' :< End)
				)
				('V' :< Both
					(' ' :< End)
					('3' :< End)
				)
			)
			('U' :< Both
				('F' :< End)
				(' ' :< Right
					('2' :< End)
				)
			)
		)
		('A' :< Both
			('R' :< Both
				('L' :< End)
				(' ' :< Left
					('+' :< End)
				)
			)
			('W' :< Both
				('P' :< End)
				('J' :< Right
					('1' :< End)
				)
			)
		)
	)
	('T' :< Both
		('N' :< Both
			('D' :< Both
				('B' :< Both
					('6' :< End)
					('=' :< End)
				)
				('X' :< Left
					('/' :< End)
				)
			)
			('K' :< Both ('C' :< End) ('Y' :< End))
		)
		('M' :< Both
			('G' :< Both
				('Z' :< Left
					('7' :< End)
				)
				('Q' :< End))
			('O' :< Both
				(' ' :< Left ('8' :< End))
				(' ' :< Both ('9':< End) ('0' :< End))
			)
		)
	)

left :: Binary a -> Maybe (Binary a)
left (_ :< End) = Nothing
left (_ :< Left x) = Just x
left (_ :< Right _) = Nothing
left (_ :< Both x _) = Just x

right :: Binary a -> Maybe (Binary a)
right (_ :< End) = Nothing
right (_ :< Right x) = Just x
right (_ :< Left _) = Nothing
right (_ :< Both _ x) = Just x

main = print "typechecked"
