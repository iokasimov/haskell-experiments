import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)

type Prices = Nonempty Stack Int

step :: Prices -> Maybe :. Nonempty Stack := Int :*: Int
step prices = (:*:) buy <$$> sales where

	buy :: Int
	buy = extract prices

	sales :: Maybe :. Nonempty Stack := Int
	sales = deconstruct prices

--------------------------------------------------------------------------------

example :: Prices
example = insert 10 $ insert 7 $ insert 5 $ insert 8 $ insert 11 $ point 9
-- example = insert 1 $ insert 3 $ insert 2 $ insert 8 $ insert 4 $ point 10

-- x = run $ Comprehension example >>= Comprehension . insert 0 . insert % empty
-- x = (:*:) <$> Comprehension example <*> Comprehension example

listify :: [a] -> Nonempty Stack a -> [a]
listify r (Construct x (Just next)) = listify (x : r) next
listify r (Construct x Nothing) = x : r

instance Covariant [] where
	f <$> [] = []
	f <$> (x : xs) = (f x) : (f <$> xs)

instance Traversable [] where
	[] ->> _ = point []
	(x : xs) ->> f = (:) <$> f x <*> xs ->> f

deriving instance (Show a, Show b) => Show (a :*: b)
deriving instance Show a => Show (Maybe a)

main = do
	print "typechecked"
	(listify [] <$$> listify [] (example =>> step)) ->> print
