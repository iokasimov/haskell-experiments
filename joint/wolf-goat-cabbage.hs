import "base" Data.Bool (bool)
import "base" Data.Functor (($>))
import "base" Data.List (delete)
import "base" Control.Applicative ((<|>))
import "lens" Control.Lens (Lens', view, _1, _2, _3, (.~), (%~))
import "joint" Control.Joint (Liftable, Stateful, State, lift
	, current, modify, nothing, run, type (:>), type (:=), (<$$>), (<**>))

data Character = Wolf | Goat | Cabbage
	deriving (Eq, Show)

instance Ord Character where
	compare Wolf Goat = GT
	compare Goat Wolf = LT
	compare Goat Cabbage = GT
	compare Cabbage Goat = LT
	compare _ _ = EQ

coexist :: Ord a => a -> a -> Bool
coexist x y = compare x y == EQ

data Direction = Back | Forward

route :: [Direction]
route = iterate alter Forward

alter :: Direction -> Direction
alter Back = Forward
alter Forward = Back

type River = ([Character], [Character])

type Iterable = Liftable []

coast :: Direction -> Lens' River [Character]
coast Back = _1
coast Forward = _2

source, target :: Direction -> Lens' River [Character]
source = coast . alter
target = coast

step :: Direction -> State River :> [] := Maybe Character
step direction = bank >>= next >>= transport where

	bank :: (Functor t, Stateful River t) => t [Character]
	bank = view (source direction) <$> current

	next :: Iterable t => [Character] -> t (Maybe Character)
	next coast = lift $ filter valid $ Nothing : (Just <$> coast) where

		valid :: Maybe Character -> Bool
		valid Nothing = and $ coexist <$> coast <*> coast
		valid (Just x) = and $ coexist <$> delete x coast <*> delete x coast

	transport Nothing = pure Nothing
	transport (Just character) = modify @River (leave . land) $> Just character where

		leave, land :: River -> River
		leave = source direction %~ delete character
		land = target direction %~ (character :)

start :: River
start = ([Goat, Wolf, Cabbage], [])

main = traverse print $ filter ((==) [] . fst . fst) $ run (traverse step $ take 7 route) start
