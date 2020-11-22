import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, Int, Show, print, (<>))

type Status = Boolean

type Field = Zipper Stream Status

instance {-# OVERLAPS #-} Extendable (Tap (Delta <:.> Stream)) where
	zs@(Tap x (TU (bs :^: fs))) =>> f = f <$> zs_duplicate zs

zs_duplicate :: Zipper Stream a -> Zipper Stream (Zipper Stream a)
zs_duplicate z = Tap z . TU $
	(extract . deconstruct $ iterate (point . rotate @Left) z)
		:^: (extract . deconstruct $ iterate (point . rotate @Right) z)

type Neighbours = Status :*: Status :*: Status

start :: Field
start = Tap True . TU $ desert :^: desert where

	desert :: Stream Status
	desert = repeat False

rule50 :: Neighbours -> Status
rule50 (True :*: True :*: True) = False
rule50 (True :*: True :*: False) = False
rule50 (True :*: False :*: True) = True
rule50 (True :*: False :*: False) = True
rule50 (False :*: True :*: True) = False
rule50 (False :*: True :*: False) = False
rule50 (False :*: False :*: True) = True
rule50 (False :*: False :*: False) = False

neighbourhood :: Field -> Neighbours
neighbourhood z = extract (sub @Left ^. z) :*: extract z :*: extract (sub @Right ^. z)

deriving instance Show Boolean
deriving instance (Show a, Show b) => Show (a :*: b)
deriving instance Show a => Show (Maybe a)
deriving instance (Show e, Show a) => Show (Conclusion e a)

instance Monotonic (Construction Identity a) a where
	bypass f r ~(Construct x (Identity xs)) = f x $ bypass f r xs

main = do
	print . neighbourhood $ start
	print . neighbourhood $ start =>> rule50 . neighbourhood
	print . neighbourhood $ start =>> rule50 . neighbourhood =>> rule50 . neighbourhood
	print . neighbourhood $ start =>> rule50 . neighbourhood =>> rule50 . neighbourhood =>> rule50 . neighbourhood
	print . neighbourhood $ start =>> rule50 . neighbourhood =>> rule50 . neighbourhood =>> rule50 . neighbourhood =>> rule50 . neighbourhood
