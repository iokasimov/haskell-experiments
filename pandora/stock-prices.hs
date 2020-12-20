import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import Prelude (Int, Show, print)
import qualified Prelude as P

data Action a = Buy a | Sell a

-- order :: Stream := Operation ()
-- order = point . alter .-+ Buy () where
--
-- 	alter :: Operation ~> Operation
-- 	alter (Buy x) = Sell x
-- 	alter (Sell x) = Buy x

--------------------------------------------------------------------------------

-- example :: Nonempty Stack Int
-- example = insert 1 $ insert 3 $ insert 2 $ insert 8 $ insert 4 $ Construct 10 Nothing

example :: Stack Int
example = insert 1 $ insert 3 $ insert 2 $ insert 8 $ insert 4 $ insert 10 $ empty

main = print "Work in progress..."
