module Main where

import Control.Lens (element, (^?), (&))
import Control.Joint (Stateful, Failable, State, current, modify, failure, run, type (:>), type (:=))

data Shape = Opened | Closed deriving (Eq, Show)

data Style = Round | Square | Angle | Curly deriving (Eq, Show)

data Symbol = Nevermind | Bracket Style Shape deriving Show

type Index = Integer

indexate :: Stateful Index t => t ()
indexate = modify (+ 1)

data Stumble
	= Deadend Style Index -- Closed bracket without opened one
	| Logjam Style Index -- Opened bracket without closed one
	| Mismatch Style Index Style Index -- Closed bracket doesn't match opened one
	deriving Show

data Memorized (s :: Shape) = Memorized Style Index

type Openings = [Memorized Opened]

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) True x _ = x
(?) False _ y = y

pass :: Applicative t => t ()
pass = pure ()

mismatch :: (Monad t, Stateful Index t, Failable Stumble t) => Style -> Style -> Index -> t ()
mismatch c o oix = current @Index >>= failure . Mismatch o oix c

logjam :: Failable Stumble t => Style -> Index -> t ()
logjam s i = failure $ Logjam s i

deadend :: (Monad t, Stateful Index t, Failable Stumble t) => Style -> t ()
deadend c = current @Index >>= failure . Deadend c

hold :: Style -> (Monad t, Stateful Index t, Stateful Openings t) => t ()
hold style = current @Index >>= modify @Openings . (:) . Memorized style

recall :: (Monad t, Stateful Openings t) => t r -> (Style -> Index -> t r) -> t r
recall on_empty f = (^? element 0) <$> current @Openings >>=
	maybe on_empty (\(Memorized style index) -> f style index)

conjoined :: Stateful Openings t => t ()
conjoined = modify @Openings tail

match :: (Monad t, Stateful Index t, Stateful Openings t, Failable Stumble t) => Style -> Style -> Index -> t ()
match closed opened oix = closed == opened ? conjoined $ mismatch closed opened oix

inspect :: Symbol -> State Index :> State Openings :> Either Stumble := ()
inspect Nevermind = pass
inspect (Bracket opened Opened) = hold opened
inspect (Bracket closed Closed) = recall (deadend closed) (match closed)

check :: Traversable s => s Symbol -> IO ()
check code = traverse (\s -> indexate *> inspect s) code *> recall pass logjam
	& flip run 1 & flip run [] & either print (const $ print "OK")

example_ok, example_mismatch, example_deadend, example_logjam :: [Symbol]
example_ok = Bracket Curly Opened : Nevermind : Bracket Curly Closed : [] -- {x}
example_mismatch = Bracket Curly Opened : Bracket Square Closed : [] -- {]
example_deadend = Bracket Round Closed : [] -- )
example_logjam = Bracket Angle Opened : [] -- <

main = check example_mismatch
