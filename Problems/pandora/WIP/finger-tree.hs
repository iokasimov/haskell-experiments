import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Char (Char)
import "base" System.IO (print)

import Gears.Instances ()

data Node23 a = Node3 a a a | Node2 a a

instance Covariant Node23 where
	f <$> Node3 x y z = Node3 (f x) (f y) (f z)
	f <$> Node2 x y = Node2 (f x) (f y)

instance Traversable Node23 where
	Node3 x y z ->> f = Node3 <$> f x <*> f y <*> f z
	Node2 x y ->> f = Node2 <$> f x <*> f y

data Affix a = A1 a | A2 a a | A3 a a a | A4 a a a a

instance Covariant Affix where
	f <$> A4 x y z v = A4 # f x # f y # f z # f v
	f <$> A3 x y z = A3 # f x # f y # f z
	f <$> A2 x y = A2 # f x # f y
	f <$> A1 x = A1 # f x

instance Traversable Affix where
	A4 x y z v ->> f = A4 <$> f x <*> f y <*> f z <*> f v
	A3 x y z ->> f = A3 <$> f x <*> f y <*> f z
	A2 x y ->> f = A2 <$> f x <*> f y
	A1 x ->> f = A1 <$> f x

data Finger a = Stop | Pin a | Deep (Affix a) (Finger (Node23 a)) (Affix a)

instance Covariant Finger where
	f <$> Stop = Stop
	f <$> Pin x = Pin $ f x
	f <$> Deep prefix subtree suffix = Deep (f <$> prefix) (f <$$> subtree) (f <$> suffix)

instance Traversable Finger where
	Stop ->> _ = point Stop
	Pin x ->> f = Pin <$> f x
	Deep prefix subtree suffix ->> f =
		Deep <$> prefix ->> f <*> subtree ->>> f <*> suffix ->> f

-- "thisisnotatree"
example :: Finger Char
example = Deep
	(A4 't' 'h' 'i' 's')
	(Deep
		(A1 (Node3 ' ' 'i' 's'))
		Stop
		(A3 (Node3 ' ' 'n' 'o') (Node2 't' ' ') (Node3 ' ' 'a' ' '))
	)
	(A4 't' 'r' 'e' 'e')

----------------------------------------------------------------------------------------------------

data Axis a d = Toe | Axis (Affix a) d (Affix a)

newtype Annexed t a = Annexed (Construction (t a) a)

instance Interpreted (Annexed t) where
	type Primary (Annexed t) a = Construction (t a) a
	run (Annexed x) = x
	unite = Annexed

type Finger' = Annexed Axis

instance Covariant Finger' where
	f <$> Annexed (Construct x Toe) = Annexed $ Construct # f x # Toe
	f <$> Annexed (Construct x (Axis prefix deep suffix)) = Annexed
		$ Construct (f x) (Axis # f <$> prefix # run (f <$> Annexed deep) # f <$> suffix)

instance Traversable Finger' where
	Annexed (Construct x Toe) ->> f = Annexed . Construct % Toe <$> f x
	Annexed (Construct x (Axis prefix deep suffix)) ->> f =
		Annexed <$> (Construct <$> f x <*> (Axis <$> prefix ->> f <*> (run <$> Annexed deep ->> f) <*> suffix ->> f))

example' :: Finger' Char
example' = Annexed . Construct 't' $ Axis
	(A1 'h')
	(Construct 'i' $ Toe)-- Axis (A1 # leaf 's') () ())
	(A3 'r' 'e' 'e')

-- leaf = Construct % Toe

main = void $ example ->> print
