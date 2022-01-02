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
type Trace = Index :*: List Token

type Checking t = (Covariant (->) (->) t, Applicative t, Monad (->) t, Stateful Trace t, Failable (Wye Token) t)

inspect :: Checking t => Sign -> t ()
inspect s = decide s .-*- (zoom @Trace # access @Index # overlook (modify @Index (+ one)))

decide :: Checking t => Sign -> t ()
decide (Bracket Opened opened) = remember opened
decide (Bracket Closed closed) = latest # deadend closed # juxtapose closed
decide _ = pass

remember :: Checking t => Bracket -> t ()
remember style = void . zoom @Trace (access @(List Token)) . overlook . modify . item @Push @List
	=<< (:*:) style . extract <-|- zoom @Trace (access @Index) (overlook (current @Index))

latest :: Checking t => t r -> (Index -> Bracket -> t r) -> t ()
latest on_empty f = void ! resolve @Token @(Maybe Token) (f |-) on_empty
	=<< (zoom @Trace # access @(List Token) >>> sub @Root >>> access @Token # current)

juxtapose :: forall t . Checking t => Bracket -> Index -> Bracket -> t ()
juxtapose closed oi opened = closed != opened ? mismatch closed opened oi ! forget where

	forget :: t ()
	forget = void ! zoom @Trace # access @(List Token) # overlook (modify @(List Token) (extract . (view # sub @Tail)))

mismatch :: Checking t => Bracket -> Bracket -> Index -> t ()
mismatch closed opened oi = failure . Both (opened :*: oi) . (closed :*:) . extract @Identity =<< zoom @Trace # access @Index # current

deadend :: Checking t => Bracket -> t ()
deadend closed = failure @(Wye Token) . Right . (closed :*:) . extract @Identity =<< zoom @Trace # access @Index # current

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
