import "base" Data.Functor (($>))
import "base" Data.List (delete)
import "lens" Control.Lens (Lens', view, _1, _2, (%~))
import "joint" Control.Joint (Liftable, Stateful, State, lift
	, current, modify, run, type (:>), type (:=))

data Character = Wolf | Goat | Cabbage deriving Eq

instance Show Character where
	show Wolf = "🐺"
	show Goat = "🐐"
	show Cabbage = "🥬"

instance Ord Character where
	compare Wolf Goat = GT
	compare Goat Wolf = LT
	compare Goat Cabbage = GT
	compare Cabbage Goat = LT
	compare _ _ = EQ

coexist :: Ord a => a -> a -> Bool
coexist x y = compare x y == EQ

data Direction = Back | Forward

type River = ([Character], [Character])

type Iterable = Liftable []

source, target :: Direction -> Lens' River [Character]
source Back = _2
source Forward = _1
target Back = _1
target Forward = _2

step :: Direction -> State River :> [] := Maybe Character
step direction = bank >>= next >>= transport where

	bank :: (Functor t, Stateful River t) => t [Character]
	bank = view (source direction) <$> current

	next :: Iterable t => [Character] -> t (Maybe Character)
	next characters = lift $ filter valid $ Nothing : (Just <$> characters) where

		valid :: Maybe Character -> Bool
		valid Nothing = and $ coexist <$> characters <*> characters
		valid (Just c) = and $ coexist <$> delete c characters <*> delete c characters

	transport Nothing = pure Nothing
	transport (Just character) = modify @River (leave . land) $> Just character where

		leave, land :: River -> River
		leave = source direction %~ delete character
		land = target direction %~ (character :)

start :: River
start = ([Goat, Wolf, Cabbage], [])

route :: [Direction]
route = iterate alter Forward where

	alter :: Direction -> Direction
	alter Back = Forward
	alter Forward = Back

main = traverse print $ filter ((==) [] . fst . fst)
	$ run (traverse step $ take 7 route) start
