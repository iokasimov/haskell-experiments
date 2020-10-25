module Main where

import Control.Lens (element, (^?), (&))
import Control.Joint (Stateful, Failable, State, current, modify, failure, zoom, run, _1, _2, type (:>), type (:=))

data Shape = Opened | Closed deriving (Eq, Show)

data Style = Round | Square | Angle | Curly deriving (Eq, Show)

data Symbol = Nevermind | Bracket Style Shape deriving Show

type Index = Integer

data Stumble
	= Deadend Style Index -- Closed bracket without opened one
	| Logjam Style Index -- Opened bracket without closed one
	| Mismatch Style Index Style Index -- Closed bracket doesn't match opened one
	deriving Show

data Memorized (s :: Shape) = Memorized Style Index

type Openings = [Memorized Opened]

type Trace = (Index, Openings)

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) True x _ = x
(?) False _ y = y

check :: Traversable s => s Symbol -> IO ()
check code = traverse (\s -> indexate *> inspect s) code *> recall pass logjam
	& flip run (1, []) & either print (const $ print "OK") where

	indexate :: Stateful Trace t => t ()
	indexate = zoom @Trace _1 $ modify @Index (+ 1)

	inspect :: Symbol -> State Trace :> Either Stumble := ()
	inspect Nevermind = pass
	inspect (Bracket opened Opened) = hold opened
	inspect (Bracket closed Closed) = recall (deadend closed) (match closed)

	logjam :: Failable Stumble t => Style -> Index -> t ()
	logjam s i = failure $ Logjam s i

	hold :: Style -> (Monad t, Stateful Trace t) => t ()
	hold style = zoom @Trace _1 (current @Index) >>=
		zoom @Trace _2 . modify @Openings . (:) . Memorized style

	recall :: (Monad t, Stateful Trace t) => t r -> (Style -> Index -> t r) -> t r
	recall on_empty f = (^? element 0) <$> zoom @Trace _2 (current @Openings) >>=
		maybe on_empty (\(Memorized style index) -> f style index)

	deadend :: (Monad t, Stateful Trace t, Failable Stumble t) => Style -> t ()
	deadend c = zoom @Trace _1 (current @Index) >>= failure . Deadend c

	match :: (Monad t, Stateful Trace t, Failable Stumble t) => Style -> Style -> Index -> t ()
	match closed opened oix = closed == opened ? conjoined $ mismatch where

		mismatch :: (Monad t, Stateful Trace t, Failable Stumble t) => t ()
		mismatch = zoom @Trace _1 (current @Index) >>= failure . Mismatch opened oix closed

		conjoined :: Stateful Trace t => t ()
		conjoined = zoom @Trace _2 $ modify @Openings tail

	pass :: Applicative t => t ()
	pass = pure ()

example_ok, example_mismatch, example_deadend, example_logjam :: [Symbol]
example_ok = Bracket Curly Opened : Nevermind : Bracket Curly Closed : [] -- {x}
example_mismatch = Bracket Curly Opened : Bracket Square Closed : [] -- {]
example_deadend = Bracket Round Closed : [] -- )
example_logjam = Bracket Angle Opened : [] -- <

main = do
	check example_ok
	check example_mismatch
	check example_deadend
	check example_logjam
