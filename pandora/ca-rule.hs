import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Char, Show, print)

start :: Zipper Stack Boolean
start = Tap True . TU $ point False :^: point False where

	desert :: Stack Boolean
	desert = linearize $ repeat False

rule50 :: (Boolean :*: Boolean :*: Boolean) -> Boolean
rule50 (True :*: True :*: True) = False
rule50 (True :*: True :*: False) = False
rule50 (True :*: False :*: True) = True
rule50 (True :*: False :*: False) = True
rule50 (False :*: True :*: True) = False
rule50 (False :*: True :*: False) = False
rule50 (False :*: False :*: True) = True
rule50 (False :*: False :*: False) = False

something :: Zipper Stack Boolean -> Maybe (Boolean :*: Boolean :*: Boolean)
something z = (\l x r -> l :*: x :*: r) <$> (extract <$> backward z) <*> Just (extract z) <*> (extract <$> forward z)

deriving instance Show Boolean
deriving instance (Show a, Show b) => Show (a :*: b)
deriving instance Show a => Show (Maybe a)

main = do
	print $ something start
	print $ rule50 <$> something start
