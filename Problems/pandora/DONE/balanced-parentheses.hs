{-# LANGUAGE UndecidableInstances #-}

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
import "base" System.IO (IO, print)
import "base" Text.Show (Show)

import Gears.Instances ()
import Gears.Utils (empty_list)

type Index = Int

type Open = Bracket :*: Index

data Stumble
	= Deadend Bracket Index -- Closed bracket without opened one
	| Logjam Bracket Index -- Opened bracket without closed one
	| Mismatch Bracket Index Bracket Index -- Closed bracket doesn't match opened one

type Opened = List Open

type Trace = Index :*: Opened

type Checking t = (Covariant (->) (->) t, Applicative t, Monad (->) t, Stateful Trace t, Failable Stumble t)

inspect :: Checking t => Sign -> t ()
inspect s = (decide s !.) =<< indexate

indexate :: Checking t => t Index
indexate = extract @Identity <-|- (zoom @Trace # access @Index # overlook (modify @Index (+ one)))

decide :: Checking t => Sign -> t ()
decide (Bracket Opened opened) = void # remember opened
decide (Bracket Closed closed) = latest # deadend closed # juxtapose closed
decide _ = pass

--remember :: Checking t => Bracket -> t ()
--remember style = void . zoom @Trace (access @Opened) . overlook . replace =<< (item @Push
	-- <-|- ((:*:) style . extract <-|- zoom @Trace (access @Index) (overlook (current @Index)))
	-- <-*- zoom @Trace # access @Opened # overlook current :: _)

remember :: Checking t => Bracket -> t (Identity Opened)
remember style = zoom @Trace (access @Opened) . overlook . modify . item @Push
	=<< (:*:) style . extract <-|- zoom @Trace (access @Index) (overlook (current @Index))

-- TODO: it looks too complicated
latest :: Checking t => t r -> (Index -> Bracket -> t r) -> t ()
latest on_empty f = void $ resolve @Open @(Maybe Open) (f |-) on_empty . extract @Identity
	=<< (zoom @Trace (access @Opened) $ overlook (extract @Identity <-|-|- zoom @Opened (sub @Root) current))

juxtapose :: Checking t => Bracket -> Index -> Bracket -> t ()
juxtapose closed oi opened = closed != opened ? mismatch closed opened oi
	$ void $ zoom @Trace # access @Opened # overlook (modify @Opened (extract . (view # sub @Tail)))

mismatch :: Checking t => Bracket -> Bracket -> Index -> t ()
mismatch closed opened oi = failure . Mismatch opened oi closed . extract @Identity =<< zoom @Trace # access @Index # current

logjam :: Checking t => Index -> Bracket -> t ()
logjam i = failure . Logjam % i

deadend :: Checking t => Bracket -> t ()
deadend closed = failure . Deadend closed . extract @Identity =<< zoom @Trace # access @Index # current

--------------------------------------------------------------------------------

type Checker = State (Index :*: Opened) :> Conclusion Stumble

check :: Traversable (->) (->) s => s Sign -> Checker ()
check code = latest pass logjam -*- inspect <<- code

interpret :: Checker () -> IO ()
interpret result = result & run % (1 :*: empty_list) & conclusion print (print "OK" !.)

deriving instance Show Bracket
deriving instance Show Sign
deriving instance Show Stumble
deriving instance Show Quote
deriving instance Show Slash
deriving instance Show Position

example_ok, example_mismatch, example_deadend, example_logjam :: List Sign
example_ok = item @Push (Bracket Opened Curly) $ item @Push Underscore $ item @Push (Bracket Closed Curly) $ empty_list  -- {_}
example_mismatch = item @Push (Bracket Opened Curly) $ item @Push (Bracket Closed Square) $ empty_list -- {]
example_deadend = item @Push (Bracket Closed Round) $ empty_list -- )
example_logjam = item @Push (Bracket Opened Angle) $ empty_list -- <

main = do
	interpret $ check example_ok
	interpret $ check example_mismatch
	interpret $ check example_deadend
	interpret $ check example_logjam
