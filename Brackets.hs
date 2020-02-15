module Main where

import Control.Lens (element, (^?), (&))
import Control.Joint (Stateful, Failable, State, current, modify, failure, lift, run, type (:>), type (~>))

data Shape = Opened | Closed deriving (Eq, Show)

data Style = Round | Square | Angle | Curly deriving (Eq, Show)

data Symbol = Nevermind | Bracket Style Shape deriving Show

type Index = Integer

data Stumble
	-- Closed bracket without opened one
	= Deadend Style Index
	-- Opened bracket without closed one
	| Logjam Style Index
	-- Closed bracket doesn't match opened one
	| Mismatch Style Index Style Index
	deriving Show

type Openings = [(Style, Index)]

indexate :: Stateful Index t => t ()
indexate = modify (+ 1)

type Checker = State Index :> State Openings :> Either Stumble

top :: Traversable s => s ~> Maybe
top s = s ^? element 0

after :: Checker ()
after = top <$> current @Openings >>=
    maybe (pure ()) (failure . uncurry Logjam)

walk :: Symbol -> Checker ()
walk Nevermind = pure ()
walk (Bracket opened Opened) = current @Index >>= modify @Openings . (:) . (,) opened
walk (Bracket closed Closed) = top <$> current @Openings >>= maybe
    (current @Index >>= failure . Deadend closed) (uncurry (match closed)) where

    match :: Style -> Style -> Index -> Checker ()
    match closed opened oix = if closed == opened
        then tail <$> current @Openings >>= modify . const
        else current @Index >>= failure . Mismatch opened oix closed

check :: Traversable s => s Symbol -> IO ()
check code = traverse (\s -> indexate *> walk s) code *> after
	& flip run 1 & flip run [] & either print (const $ print "OK")

example_ok, example_mismatch, example_deadend, example_logjam :: [Symbol]
example_ok = Bracket Curly Opened : Nevermind : Bracket Curly Closed : [] -- {x}
example_mismatch = Bracket Curly Opened : Bracket Square Closed : [] -- {]
example_deadend = Bracket Round Closed : [] -- )
example_logjam = Bracket Angle Opened : [] -- <

main = check example_mismatch
