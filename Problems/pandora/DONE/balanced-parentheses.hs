import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
import "base" System.IO (IO, print)
import "base" Text.Show (Show)

import Gears.Instances ()

type Index = Int
type Token = Bracket :*: Index
type Tokens = List Token
type Trace = Index :*: Tokens

type Checking t = (Covariant (->) (->) t, Applicative t, Monad (->) t, Stateful Trace t, Failable (Wye Token) t)

inspect :: Checking t => Sign -> t ()
inspect s = decide s .-*- zoom_ @Identity @Trace <--- access @Index <--- modify @State (+ one)

decide :: Checking t => Sign -> t ()
decide (Bracket Opened opened) = remember opened
decide (Bracket Closed closed) = latest # deadend closed # juxtapose closed
decide _ = pass

remember :: Checking t => Bracket -> t ()
remember style = void ! zoom_ @Identity @Trace (access @Tokens) . push @List
	=<< (:*:) style <-|- zoom_ @Identity @Trace <--- access @Index <--- get @State

latest :: Checking t => t r -> (Index -> Bracket -> t r) -> t ()
latest on_empty f = void ! resolve @Token (f |-) on_empty =<< zoom_ @Maybe @Trace <--- access @Tokens >>> top @List <--- get @State

juxtapose :: forall t . Checking t => Bracket -> Index -> Bracket -> t ()
juxtapose closed oi opened = closed != opened ? mismatch oi ! forget where

	mismatch :: Checking t => Index -> t ()
	mismatch oi = failure . Both (opened :*: oi) . (closed :*:) =<< zoom_ @Identity @Trace <--- access @Index <--- get @State

	forget :: t ()
	forget = void ! zoom_ @Identity @Trace <--- access @Tokens <--- pop @List

deadend :: Checking t => Bracket -> t ()
deadend closed = failure @(Wye Token) . Right . (closed :*:) =<< zoom_ @Identity @Trace <--- access @Index <--- get @State

logjam :: Checking t => Index -> Bracket -> t ()
logjam i b = failure # Left (b :*: i)

type Checker = State (Index :*: List Token) :> Conclusion (Wye Token)

check :: Traversable (->) (->) s => s Sign -> Checker ()
check code = latest pass logjam .-*- inspect <<- code

interpret :: Checker () -> IO ()
interpret result = result & run % (1 :*: empty) & conclusion print (constant # print "OK")

example_ok, example_mismatch, example_deadend, example_logjam :: List Sign
example_ok = item @Push (Bracket Opened Curly) . item @Push Underscore . item @Push (Bracket Closed Curly) ! empty  -- {_}
example_mismatch = item @Push (Bracket Opened Curly) . item @Push (Bracket Closed Square) ! empty -- {]
example_deadend = item @Push (Bracket Closed Round) ! empty -- )
example_logjam = item @Push (Bracket Opened Angle) ! empty -- <

main = do
	interpret ! check example_ok
	interpret ! check example_mismatch
	interpret ! check example_deadend
	interpret ! check example_logjam
