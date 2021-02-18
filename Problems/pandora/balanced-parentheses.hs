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

type Opened = Stack Open

type Trace = Index :*: Opened

type Checking t = (Applicative t, Monad t, Stateful Trace t, Failable Stumble t)

inspect :: Checking t => Symbol -> t ()
inspect s = indexate *> decide s

decide :: Checking t => Symbol -> t ()
decide (Bracket opened Opened) = keep opened
decide (Bracket closed Closed) = latest (deadend closed) (juxtapose closed)
decide Nevermind = skip

indexate :: Checking t => t ()
indexate = adjust @Trace @Index (+ one)

keep :: Checking t => Style -> t ()
keep style = adjust @Trace @Opened . (!) =<< (+=)
	<$> ((:*:) style <$> magnify @Trace @Index)
	<*> magnify @Trace @Opened

-- TODO: it looks too complicated
latest :: Checking t => t r -> (Index -> Style -> t r) -> t r
latest on_empty f = zoom @Trace (focus @Right |> focus @Head) current
	>>= resolve @Open @(Maybe Open) (|- f) on_empty

juxtapose :: Checking t => Style -> Index -> Style -> t ()
juxtapose closed oi opened = closed != opened
	? mismatch closed opened oi
	$ adjust @Trace @Opened $ view (sub @Tail)

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

type Checker = State (Index :*: Opened) :> Conclusion Stumble := ()

check :: Traversable s => s Symbol -> IO ()
check code = ((code ->> inspect) *> latest skip logjam :: Checker)
	& run % (1 :*: empty) & conclusion print ((print "OK") !)

deriving instance Show Shape
deriving instance Show Style
deriving instance Show Symbol
deriving instance Show Stumble

example_ok :: Stack Symbol -- , example_mismatch, example_deadend, example_logjam :: Stack Symbol
-- example_ok = insert (Bracket Curly Opened) $ insert Nevermind $ insert (Bracket Curly Closed) $ empty  -- {x}
example_ok = Bracket Curly Opened += Nevermind += Bracket Curly Closed += empty  -- {x}
-- example_mismatch = insert (Bracket Curly Opened) $ insert (Bracket Square Closed) $ empty -- {]
-- example_deadend = insert (Bracket Round Closed) empty -- )
-- example_logjam = insert (Bracket Angle Opened) empty -- <

main = do
	check example_ok
	-- check example_mismatch
	-- check example_deadend
	-- check example_logjam
