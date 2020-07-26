import "base" Data.Bool (bool)
import "base" Data.Functor (($>))
import "base" Data.List (delete)
import "lens" Control.Lens (Lens', view, _1, _2, (%~))
import "joint" Control.Joint (Liftable, Stateful, State, lift
	, current, modify, run, type (:>), type (:=))

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

data Move = Move Bank Character | Vain Bank
	deriving Show

type River = ([Character],[Character])

coast :: Bank -> Lens' River [Character]
coast Initial = _1
coast Far = _2

source, target :: Bank -> Lens' River [Character]
source = coast . alter
target = coast

transport :: (Applicative t, Stateful River t) => Bank -> Character -> t Move
transport bank candidate = modify @River (arrive . depart) $> Move bank candidate where

	depart, arrive :: River -> River
	depart = source bank %~ delete candidate
	arrive = target bank %~ (candidate :)

finished :: (Functor t, Stateful River t) => t (Maybe Character)
finished = last_one . view (coast Initial) <$> current where

	last_one :: [Character] -> Maybe Character
	last_one (one : []) = Just $ one
	last_one _ = Nothing

attempt :: (Monad t, Stateful River t, Liftable [] t) => Bank -> t Move
attempt bank = compatibility <$> next <*> destination
	>>= bool (pure $ Vain bank) (next >>= transport bank) where

	next :: (Monad t, Stateful River t, Liftable [] t) => t Character
	next = view (source bank) <$> current >>= lift

	compatibility :: Character -> [Character] -> Bool
	compatibility candidate others = all (coexist candidate) others

	destination :: (Functor t, Stateful River t) => t [Character]
	destination = view (target bank) <$> current

step :: Bank -> State River :> [] := Move
step Far = finished >>= maybe (attempt Far) (transport Far)
step Initial = attempt Initial

start :: River
start = ([Wolf, Goat, Cabbage], [])

main = traverse print $ run (traverse step $ take 5 route) start
