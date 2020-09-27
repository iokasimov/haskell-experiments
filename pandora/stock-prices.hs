import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
-- import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)
import qualified Prelude as P

type Prices = Nonempty Stack Int

prices :: Prices
prices = insert 10 $ insert 7 $ insert 5 $ insert 8 $ insert 11 $ Construct 9 Nothing

main = print "Work in progress..."
