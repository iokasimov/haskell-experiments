
import "base" Control.Monad (guard)
import "base" Data.List (delete)
import "lens" Control.Lens (Lens', view, _1, _2, (&), (.~), (%~))
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
route = iterate alter Far

alter :: Bank -> Bank
alter Initial = Far
alter Far = Initial

data Move = Move Bank Character

type River = ([Character],[Character])

coast :: Bank -> Lens' River [Character]
coast Initial = _1
coast Far = _2

source, target :: Bank -> Lens' River [Character]
source = coast . alter
target = coast

transport :: Stateful River t => Bank -> Character -> t ()
transport bank candidate = modify @River $ arrive . depart where

	depart = source bank %~ delete candidate
	arrive = target bank %~ (candidate :)

step :: Bank -> State River :> [] := ()
step bank = do
	from <- view (source bank) <$> current
	candidate <- lift from
	to <- view (target bank) <$> current
	lift @[] . guard $ all (coexist candidate) to
	transport bank candidate

start :: River
start = ([Wolf, Goat, Cabbage], [])

main = print $ run (step Far) start
