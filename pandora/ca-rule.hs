import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, Show, print)

type Status = Boolean

type Field = Zipper Stream Status

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

-- neighbourhood :: Field -> Neighbours
-- neighbourhood z = leftward :*: extract z :*: rightward where
--
-- 	leftward, rightward :: Status
-- 	leftward = vacant $ extract <$> backward' z
-- 	rightward = vacant $ extract <$> forward' z
--
-- 	vacant :: Maybe Status -> Status
-- 	vacant = maybe False identity

deriving instance Show Boolean
deriving instance (Show a, Show b) => Show (a :*: b)
deriving instance Show a => Show (Maybe a)
deriving instance (Show e, Show a) => Show (Conclusion e a)

instance Monotonic (Construction Identity a) a where
	bypass f r ~(Construct x (Identity xs)) = f x $ bypass f r xs

main = do
    print . extract $ repeat False
    -- print . find (equate False) $ True :*: False :*: True
    -- print . find (equate False) $ insert True $ insert True $ insert False $ empty @Stack

-- main = print . extract $ start
	-- print . extract $ start =>> rule50 . neighbourhood
	-- print . extract $ start =>> rule50 . neighbourhood =>> rule50 . neighbourhood
	-- print . extract $ start =>> rule50 . neighbourhood =>> rule50 . neighbourhood =>> rule50 . neighbourhood
	-- print . extract $ start =>> rule50 . neighbourhood =>> rule50 . neighbourhood =>> rule50 . neighbourhood =>> rule50 . neighbourhood
