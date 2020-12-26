import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)

import Gears.Instances ()
--------------------------------------------------------------------------------

example :: Nonempty Stack Int
example = insert 1 $ insert 2 $ insert 3 $ point 4

-- comb = (:*:) <$> point (extract example) <*> Comprehension (TU $ deconstruct example)
comb = let stack = Comprehension . unite $ Just example in (:*:) <$> stack <*> stack

example_zipper :: Zipper Stack Int
example_zipper = Tap 1 . TU $ empty :^: unite (deconstruct example)

-- enumerate :: Nonempty Stack Int ->
-- enumerate (TU Just $ nonempty) = Left $ Construct (extract nonempty) $ enumerate ()
-- enumerate (TU Nothing) = End

main = void $ do
	example ->> print
