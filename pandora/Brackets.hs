import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import qualified Prelude as Base (Integer, IO, Show, print, (+))

data Shape = Opened | Closed

data Style = Round | Square | Angle | Curly

instance Setoid Style where
	Round == Round = True
	Square == Square = True
	Angle == Angle = True
	Curly == Curly = True
	_ == _ = False

data Symbol = Bracket Style Shape | Nevermind

type Index = Base.Integer

indexate :: (Covariant t, Stateful Index t) => t ()
indexate = modify (Base.+ (1 :: Base.Integer))

data Stumble
	= Deadend Style Index -- Closed bracket without opened one
	| Logjam Style Index -- Opened bracket without closed one
	| Mismatch Style Index Style Index -- Closed bracket doesn't match opened one

pass :: Pointable t => t ()
pass = point ()

mismatch :: (Bindable t, Stateful Index t, Failable Stumble t) => Style -> Style -> Index -> t ()
mismatch c o oix = current >>= failure . Mismatch o oix c

logjam :: (Covariant t, Failable Stumble t) => Index -> Style -> t ()
logjam i s = failure $ Logjam s i

deadend :: (Bindable t, Stateful Index t, Failable Stumble t) => Style -> t ()
deadend c = current >>= failure . Deadend c

type Openings = Stack Open

type Open = Style :*: Index

hold :: Style -> (Bindable t, Stateful Index t, Stateful Openings t) => t ()
hold style = current >>= modify . push @Open . (style :*:)

previous :: (Bindable t, Stateful Openings t) => t r -> (Index -> Style -> t r) -> t r
previous on_empty f = view (top @Open) <$> current >>= maybe on_empty (|- f)

match :: (Bindable t, Stateful Index t, Stateful Openings t, Failable Stumble t) => Style -> Index -> Style -> t ()
match closed oix opened = closed == opened ? modify (pop @Open) $ mismatch closed opened oix

inspect :: Symbol -> State Index :> State Openings :> Conclusion Stumble := ()
inspect Nevermind = pass
inspect (Bracket opened Opened) = hold opened
inspect (Bracket closed Closed) = previous (deadend closed) (match closed)

deriving instance Base.Show Shape
deriving instance Base.Show Style
deriving instance Base.Show Symbol
deriving instance Base.Show Stumble

check :: Traversable s => s Symbol -> Base.IO ()
check code = traverse (\s -> indexate *> inspect s) code *> previous pass logjam
	& run % 1 & run % empty & conclusion Base.print ((Base.print "OK") !)

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
