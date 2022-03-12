import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
-- import "base" Debug.Trace (trace)
import "base" System.IO (IO, print, getLine)
import "base" Text.Show (Show)

import Gears.Instances ()

type Index = Int
type Token = Bracket :*: Index
type Tokens = List Token
type Trace = Index :*: Tokens

type Checking t = (Covariant (->) (->) t, Applicative t, Monad (->) t, Stateful Trace t, Failable (Wye Token) t)

inspect :: Checking t => Sign -> t ()
inspect s = decide s .-*- zoom (access @Index @Trace) (modify @State (+ one))

decide :: Checking t => Sign -> t ()
decide (Bracket Opened opened) = remember <-- opened
decide (Bracket Closed closed) = latest <-- deadend closed <-- juxtapose closed
decide _ = pass

remember :: Checking t => Bracket -> t ()
remember style = void <---- zoom (access @Tokens @Trace) . push @List . (:*:) style
	==<< zoom <-- access @Index @Trace <-- get @State

latest :: Checking t => t r -> (Index -> Bracket -> t r) -> t ()
latest on_empty on_remaining = void <---- resolve @Token (on_remaining |-) on_empty
	==<< zoom (access @Tokens @Trace >>> top @List) (get @State)

juxtapose :: Checking t => Bracket -> Index -> Bracket -> t ()
juxtapose closed oi opened = closed ?= opened
	<---- zoom <--- access @Tokens @Trace <--- void <-- pop @List
	<---- failure . Both (opened :*: oi) . (closed :*:)
		==<< zoom <-- access @Index @Trace <-- get @State

deadend :: Checking t => Bracket -> t ()
deadend closed = failure . Right . (closed :*:)
	 ===<< zoom <-- access @Index @Trace <-- get @State

type Checker = State (Index :*: List Token) :> Conclusion (Wye Token)

check :: Traversable (->) (->) s => s Sign -> Checker ()
check code = latest pass (failure @(Wye Token) .:.. (Left -|)) .-*- void (inspect <<-- code)

interpret :: Checker () -> IO ()
interpret result = conclusion
	<---- print
	<---- constant <-- print "OK"
	<---- result <~~~ 1 :*: empty

example_ok, example_mismatch, example_deadend, example_logjam :: List Sign
example_ok = item @Push (Bracket Opened Curly) . item @Push Underscore . item @Push (Bracket Closed Curly) <-- empty  -- {_}
example_mismatch = item @Push (Bracket Opened Curly) . item @Push (Bracket Closed Square) <-- empty -- {]
example_deadend = item @Push (Bracket Closed Round) <-- empty -- )
example_logjam = item @Push (Bracket Opened Angle) <-- empty -- <

main = do
	interpret <-- check example_ok
	interpret <-- check example_mismatch
	interpret <-- check example_deadend
	interpret <-- check example_logjam
