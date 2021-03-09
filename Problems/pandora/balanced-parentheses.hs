import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (IO, Int, Show (show), print)
import Gears.Instances ()

data Shape = Opened | Closed

data Style = Round | Square | Angle | Curly

instance Setoid Style where
	Round == Round = True
	Square == Square = True
	Angle == Angle = True
	Curly == Curly = True
	_ == _ = False

data Symbol = Bracket Style Shape | Nevermind

type Index = Int

type Open = Style :*: Index

data Stumble
	= Deadend Style Index -- Closed bracket without opened one
	| Logjam Style Index -- Opened bracket without closed one
	| Mismatch Style Index Style Index -- Closed bracket doesn't match opened one

type Opened = List Open

type Trace = Index :*: Opened

type Checking t = (Applicative t, Monad t, Stateful Trace t, Failable Stumble t)

inspect :: Checking t => Symbol -> t ()
inspect s = indexate *> decide s

indexate :: Checking t => t Index
indexate = adjust @Trace @Index (+ one)

decide :: Checking t => Symbol -> t ()
decide (Bracket opened Opened) = remember opened
decide (Bracket closed Closed) = latest (deadend closed) (juxtapose closed)
decide Nevermind = skip

remember :: Checking t => Style -> t ()
remember style = void . adjust @Trace @Opened . (!) =<< item @Push
	<$> ((:*:) style <$> magnify @Trace @Index)
	<*> magnify @Trace @Opened

-- TODO: it looks too complicated
latest :: Checking t => t r -> (Index -> Style -> t r) -> t ()
latest on_empty f = void $ zoom @Trace (focus @Right |> focus @Head) current
	>>= resolve @Open @(Maybe Open) (|- f) on_empty

juxtapose :: Checking t => Style -> Index -> Style -> t ()
juxtapose closed oi opened = closed != opened ? mismatch closed opened oi
	$ void . adjust @Trace @Opened $ view / sub @Tail

skip :: Pointable t => t ()
skip = point ()

mismatch :: Checking t => Style -> Style -> Index -> t ()
mismatch closed opened oi = magnify @Trace @Index
	>>= failure . Mismatch opened oi closed

logjam :: Checking t => Index -> Style -> t ()
logjam i = failure . Logjam % i

deadend :: Checking t => Style -> t ()
deadend closed = magnify @Trace @Index >>= failure . Deadend closed

--------------------------------------------------------------------------------

type Checker = State (Index :*: Opened) :> Conclusion Stumble

check :: Traversable s => s Symbol -> Checker ()
check code = code ->> inspect *> latest skip logjam

interpret :: Checker () -> IO ()
interpret result = result & run % (1 :*: empty) & conclusion print ((print "OK") !)

deriving instance Show Shape
deriving instance Show Style
deriving instance Show Symbol
deriving instance Show Stumble

example_ok, example_mismatch, example_deadend, example_logjam :: List Symbol
example_ok = item @Push (Bracket Curly Opened) $ item @Push Nevermind $ item @Push (Bracket Curly Closed) $ empty  -- {x}
example_mismatch = item @Push (Bracket Curly Opened) $ item @Push (Bracket Square Closed) $ empty -- {]
example_deadend = item @Push (Bracket Round Closed) $ empty -- )
example_logjam = item @Push (Bracket Angle Opened) $ empty -- <

main = do
	interpret $ check example_ok
	interpret $ check example_mismatch
	interpret $ check example_deadend
	interpret $ check example_logjam
