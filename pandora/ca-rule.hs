import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, Int, Show (show), print, (<>))
import Control.Concurrent (threadDelay)

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

five_around :: Field -> [Status]
five_around (Tap x (TU (bs :^: fs))) = last5 bs <> [x] <> first5 fs where

	first5, last5 :: Stream ~> []
	first5 (Construct x (Identity (Construct x' (Identity (Construct x'' (Identity (Construct x''' (Identity (Construct x'''' _))))))))) = [x, x', x'', x''', x'''']
	last5 (Construct x (Identity (Construct x' (Identity (Construct x'' (Identity (Construct x''' (Identity (Construct x'''' _))))))))) = [x'''', x''', x'', x', x]

instance Show Boolean where
	show True = "*"
	show False = " "

deriving instance (Show a, Show b) => Show (a :*: b)
deriving instance Show a => Show (Maybe a)
deriving instance (Show e, Show a) => Show (Conclusion e a)

instance Monotonic (Construction Identity a) a where
	bypass f r ~(Construct x (Identity xs)) = f x $ bypass f r xs

lifecycle act being = do
	threadDelay 1000000
	print $ five_around being
	lifecycle act $ being =>> act

main = lifecycle (rule50 . neighbourhood) start
