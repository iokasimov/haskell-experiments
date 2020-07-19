
import "base" Control.Monad (guard)
import "base" Data.List (delete)
import "lens" Control.Lens (Lens', view, _1, _2, (.~), (%~))
import "joint" Control.Joint (Stateful, Failable, State, lift
	, current, modify, failure, run, type (:>), type (:=))

data Character = Wolf | Goat | Cabbage
	deriving (Eq, Show)

instance Ord Character where
	compare Wolf Wolf = EQ
	compare Wolf Goat = GT
	compare Wolf Cabbage = EQ
	compare Goat Wolf = LT
	compare Goat Goat = EQ
	compare Goat Cabbage = GT
	compare Cabbage Wolf = EQ
	compare Cabbage Goat = LT
	compare Cabbage Cabbage = EQ

coexist :: Ord a => a -> a -> Bool
coexist x y = compare x y == EQ

data Bank = Initial | Far
	deriving Show

route :: [Bank]
route = iterate alter Initial

alter :: Bank -> Bank
alter Initial = Far
alter Far = Initial

data Move = Move Bank Character

type River = ([Character],[Character])

coast :: Bank -> Lens' River [Character]
coast Initial = _1
coast Far = _2

step :: Bank -> State River :> [] := ()
step bank = do
	let source = coast $ alter bank
	let target = coast bank
	from <- view source <$> current
	candidate <- lift from
	to <- view target <$> current
	lift @[] . guard $ all (coexist candidate) to
	-- I can't figure out what's wrong here...
	modify @River (source %~ delete candidate)
	-- modify (target %~ (candidate :))

main = print "Work in progress..."
