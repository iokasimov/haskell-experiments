module Main where

-- Source: https://apfelmus.nfshost.com/articles/fun-with-morse-code.html

import Prelude hiding (Either (Left, Right))
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

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

left :: Monad t => Binary a -> MaybeT t (Binary a)
left (_ :< End) = MaybeT $ pure Nothing
left (_ :< Left x) = MaybeT . pure . Just $ x
left (_ :< Right _) = MaybeT $ pure Nothing
left (_ :< Both x _) = MaybeT . pure . Just $ x

right :: Monad t => Binary a -> MaybeT t (Binary a)
right (_ :< End) = MaybeT $ pure Nothing
right (_ :< Right x) = MaybeT . pure . Just $ x
right (_ :< Left _) = MaybeT $ pure Nothing
right (_ :< Both _ x) = MaybeT . pure . Just $ x

data Morse = Dot | Dash

decode :: Morse -> StateT (Binary Char) (MaybeT IO) ()
decode Dot = get >>= lift . left >>= \t -> (lift . lift . print . extract $ t) *> put t
decode Dash = get >>= lift . right >>= \t -> (lift . lift . print . extract $ t) *> put t

a :: [Morse]
a = [Dot, Dash]

digit4 :: [Morse]
digit4 = [Dot, Dot, Dot, Dot, Dash]

main = runMaybeT $ execStateT (traverse decode digit4) dictionary
