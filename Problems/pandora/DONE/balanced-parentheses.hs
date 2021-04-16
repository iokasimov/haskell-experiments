import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (IO, Int, Show (show), print)
import Gears.Instances ()

instance Setoid Bracket where
	Round == Round = True
	Square == Square = True
	Angle == Angle = True
	Curly == Curly = True
	_ == _ = False

type Index = Int

type Open = Bracket :*: Index

data Stumble
	= Deadend Bracket Index -- Closed bracket without opened one
	| Logjam Bracket Index -- Opened bracket without closed one
	| Mismatch Bracket Index Bracket Index -- Closed bracket doesn't match opened one

type Opened = List Open

type Trace = Index :*: Opened

type Checking t = (Applicative t, Monad t, Stateful Trace t, Failable Stumble t)

inspect :: Checking t => Sign -> t ()
inspect s = indexate *> decide s

indexate :: Checking t => t Index
indexate = adjust @Trace @Index (+ one)

decide :: Checking t => Sign -> t ()
decide (Bracket Opened opened) = remember opened
decide (Bracket Closed closed) = latest (deadend closed) (juxtapose closed)
decide _ = skip

remember :: Checking t => Bracket -> t ()
remember style = void . adjust @Trace @Opened . (!) =<< item @Push
	<$> ((:*:) style <$> magnify @Trace @Index)
	<*> magnify @Trace @Opened

-- TODO: it looks too complicated
latest :: Checking t => t r -> (Index -> Bracket -> t r) -> t ()
latest on_empty f = void $ zoom @Trace (focus @Right |> focus @Head) current
	>>= resolve @Open @(Maybe Open) (|- f) on_empty

juxtapose :: Checking t => Bracket -> Index -> Bracket -> t ()
juxtapose closed oi opened = closed != opened ? mismatch closed opened oi
	$ void . adjust @Trace @Opened $ view # sub @Tail

skip :: Pointable t => t ()
skip = point ()

mismatch :: Checking t => Bracket -> Bracket -> Index -> t ()
mismatch closed opened oi = magnify @Trace @Index
	>>= failure . Mismatch opened oi closed

logjam :: Checking t => Index -> Bracket -> t ()
logjam i = failure . Logjam % i

deadend :: Checking t => Bracket -> t ()
deadend closed = magnify @Trace @Index >>= failure . Deadend closed

--------------------------------------------------------------------------------

type Checker = State (Index :*: Opened) :> Conclusion Stumble

check :: Traversable s => s Sign -> Checker ()
check code = code ->> inspect *> latest skip logjam

interpret :: Checker () -> IO ()
interpret result = result & run % (1 :*: empty) & conclusion print ((print "OK") !)

deriving instance Show Bracket
deriving instance Show Sign
deriving instance Show Stumble
deriving instance Show Quote
deriving instance Show Slash
deriving instance Show Position

example_ok, example_mismatch, example_deadend, example_logjam :: List Sign
example_ok = item @Push (Bracket Opened Curly) $ item @Push Underscore $ item @Push (Bracket Closed Curly) $ empty  -- {_}
example_mismatch = item @Push (Bracket Opened Curly) $ item @Push (Bracket Closed Square) $ empty -- {]
example_deadend = item @Push (Bracket Closed Round) $ empty -- )
example_logjam = item @Push (Bracket Opened Angle) $ empty -- <

main = do
	interpret $ check example_ok
	interpret $ check example_mismatch
	interpret $ check example_deadend
	interpret $ check example_logjam
