import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (print)

data Character = Wolf | Goat | Cabbage

instance Setoid Character where
	Wolf == Wolf = True
	Goat == Goat = True
	Cabbage == Cabbage = True
	_ == _ = False

instance Chain Character where
	Wolf <=> Wolf = Equal
	Wolf <=> Goat = Greater
	Wolf <=> Cabbage = Equal
	Goat <=> Wolf = Less
	Goat <=> Goat = Equal
	Goat <=> Cabbage = Greater
	Cabbage <=> Wolf = Equal
	Cabbage <=> Goat = Less
	Cabbage <=> Cabbage = Equal

type River = Delta (Stack Character)

start :: River
start = (push Wolf $ push Goat $ push Cabbage $ empty) :^: empty

data Bank = Initial | Far

data Move = Move Character Bank

step :: State River :> Stack := Move
step = current @River >>= \(initial :^: far) ->
	Move % Far <$> adapt initial

legal :: Chain a => a -> a -> Boolean
legal x y = x <=> y == Equal

main = print "Work in progress..."
