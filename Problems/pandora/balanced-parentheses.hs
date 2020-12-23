import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (IO, Integer, Show, print, succ, show)
import Debug.Trace (trace)

data Shape = Opened | Closed

data Style = Round | Square | Angle | Curly

instance Setoid Style where
	Round == Round = True
	Square == Square = True
	Angle == Angle = True
	Curly == Curly = True
	_ == _ = False

data Symbol = Bracket Style Shape | Nevermind

type Index = Integer

type Open = Style :*: Index

data Stumble
	= Deadend Style Index -- Closed bracket without opened one
	| Logjam Style Index -- Opened bracket without closed one
	| Mismatch Style Index Style Index -- Closed bracket doesn't match opened one

type Trace = Index :*: Stack Open

type Checking t = (Applicative t, Monad t, Stateful Trace t, Failable Stumble t)

skip :: Pointable t => t ()
skip = point ()

mismatch :: Checking t => Style -> Style -> Index -> t ()
mismatch c o oix = current @Trace >>= failure . Mismatch o oix c . attached

logjam :: Checking t => Index -> Style -> t ()
logjam i s = failure $ Logjam s i

deadend :: Checking t => Style -> t ()
deadend c = current @Trace >>= failure . Deadend c . attached

keep :: Checking t => Style -> t ()
keep style = current @Trace >>= zoom @Trace (sub @Right)
	. replace . (|- (insert %)) . ((style :*:) <-> identity)

-- FIXME: use `resolve (|- f) on_empty` instad pattern matching
latest :: Checking t => t r -> (Index -> Style -> t r) -> t r
latest on_empty f = view (focus @Head) . extract <$> current @Trace >>= \case
	Just x -> x |- f
	Nothing -> on_empty

match :: Checking t => Style -> Index -> Style -> t ()
match closed i opened = closed == opened
	? (zoom @Trace (sub @Right) . modify $ pop @Open)
		$ mismatch closed opened i

indexate :: Checking t => t ()
indexate = zoom @Trace (sub @Left) $ modify @Index succ

decide :: Checking t => Symbol -> t ()
decide (Bracket opened Opened) = keep opened
decide (Bracket closed Closed) = latest (deadend closed) (match closed)
decide Nevermind = skip

inspect :: Checking t => Symbol -> t ()
inspect s = indexate *> decide s

--------------------------------------------------------------------------------

type Checker = State (Index :*: Stack Open) :> Conclusion Stumble := ()

check :: Traversable s => s Symbol -> IO ()
check code = (code ->> inspect *> latest skip logjam :: Checker)
	& run % (1 :*: empty) & conclusion print ((print "OK") !)

deriving instance Show Shape
deriving instance Show Style
deriving instance Show Symbol
deriving instance Show Stumble

example_ok, example_mismatch, example_deadend, example_logjam :: Stack Symbol
example_ok = insert (Bracket Curly Opened) $ insert Nevermind $ insert (Bracket Curly Closed) $ empty  -- {x}
example_mismatch = insert (Bracket Curly Opened) $ insert (Bracket Square Closed) $ empty -- {]
example_deadend = insert (Bracket Round Closed) empty -- )
example_logjam = insert (Bracket Angle Opened) empty -- <

main = do
	check example_ok
	check example_mismatch
	check example_deadend
	check example_logjam
