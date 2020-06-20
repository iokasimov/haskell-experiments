import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Integer, Show, print, succ)

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

type Checking t = (Applicative t, Monad t, Stateful Index t, Failable Stumble t, Stateful (Stack Open) t)

skip :: Pointable t => t ()
skip = point ()

mismatch :: Checking t => Style -> Style -> Index -> t ()
mismatch c o oix = current >>= failure . Mismatch o oix c

logjam :: Checking t => Index -> Style -> t ()
logjam i s = failure $ Logjam s i

deadend :: Checking t => Style -> t ()
deadend c = current >>= failure . Deadend c

keep :: Checking t => Style -> t ()
keep style = current >>= modify . push @Open . (style :*:)

latest :: Checking t => t r -> (Index -> Style -> t r) -> t r
latest on_empty f = view (focus @Head) <$> current @(Stack Open) >>= maybe on_empty (|- f)

match :: Checking t => Style -> Index -> Style -> t ()
match closed i opened = closed == opened
	? modify (pop @Open) $ mismatch closed opened i

indexate :: Checking t => t ()
indexate = modify @Index succ

decide :: Checking t => Symbol -> t ()
decide (Bracket opened Opened) = keep opened
decide (Bracket closed Closed) = latest (deadend closed) (match closed)
decide Nevermind = skip

inspect :: Checking t => Symbol -> t ()
inspect s = indexate *> decide s

--------------------------------------------------------------------------------

type Checker = State Index :> State (Stack Open) :> Conclusion Stumble := ()

check :: Traversable s => s Symbol -> IO ()
check code = (code ->> inspect *> latest skip logjam :: Checker)
	& run % 1 & run % empty & conclusion print ((print "OK") !)

deriving instance Show Shape
deriving instance Show Style
deriving instance Show Symbol
deriving instance Show Stumble

example_ok, example_mismatch, example_deadend, example_logjam :: Stack Symbol
example_ok = push (Bracket Curly Opened) $ push Nevermind $ push (Bracket Curly Closed) $ empty  -- {x}
example_mismatch = push (Bracket Curly Opened) $ push (Bracket Square Closed) $ empty -- {]
example_deadend = push (Bracket Round Closed) empty -- )
example_logjam = push (Bracket Angle Opened) empty -- <

main = do
    check example_ok
    check example_mismatch
    check example_deadend
    check example_logjam
