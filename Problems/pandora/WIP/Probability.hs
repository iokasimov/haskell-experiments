import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
-- import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)

-- import Gears.Instances ()

-- data Side = Heads | Tails
--
-- instance Setoid Side where
-- 	Heads == Heads = True
-- 	Tails == Tails = True
-- 	_ == _ = False
--
-- delta_to_product :: Delta a -> Product a a
-- delta_to_product (x :^: y) = x :*: y
--
-- class Probability e a where
-- 	likelihood :: e -> a -> Delta Int
--
-- instance Setoid a => Probability a a where
-- 	likelihood event x = x == event ? 1 :^: 1 $ 0 :^: 1
--
-- instance (Setoid a, Probability a b) => Probability a (a :*: b) where
-- 	likelihood event (x :*: y) = case likelihood event y of
-- 		n :^: d -> ((x == event ? 1 $ 0) + n) :^: d + 1
--
-- deriving instance Show Side
--
-- ps = Heads :*: Tails
--
-- main = print $ likelihood Tails ps

main = print "WIP"
