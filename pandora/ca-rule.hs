import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, Show, print)

start :: Zipper Stack Boolean
start = Tap True . TU $ desert :^: desert where

	desert :: Stack Boolean
	desert = insert False $ insert False
		$ insert False $ insert False $ insert False
		$ insert False $ insert False $ insert False
		$ insert False $ insert False $ empty

rule50 :: (Boolean :*: Boolean :*: Boolean) -> Boolean
rule50 (True :*: True :*: True) = False
rule50 (True :*: True :*: False) = False
rule50 (True :*: False :*: True) = True
rule50 (True :*: False :*: False) = True
rule50 (False :*: True :*: True) = False
rule50 (False :*: True :*: False) = False
rule50 (False :*: False :*: True) = True
rule50 (False :*: False :*: False) = False

neighbours :: Zipper Stack Boolean -> Boolean :*: Boolean :*: Boolean
neighbours z = leftward :*: extract z :*: rightward where

	leftward, rightward :: Boolean
	leftward = vacant (extract <$> backward z)
	rightward = vacant (extract <$> forward z)

	vacant :: Maybe Boolean -> Boolean
	vacant = maybe False identity

deriving instance Show Boolean
deriving instance (Show a, Show b) => Show (a :*: b)
deriving instance Show a => Show (Maybe a)

main = print $ extract <$> forward start
	-- print . extract $ start =>> rule50 . neighbours
