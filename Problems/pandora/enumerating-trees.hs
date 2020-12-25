import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)

import Gears.Instances ()

-- enumerate :: Nonempty Stack Int ->
-- enumerate elements = extract elements <$> deconstruct elements

--------------------------------------------------------------------------------

example :: Nonempty Stack Int
example = insert 1 $ insert 2 $ insert 3 $ point 4

-- comb = (:*:) <$> point (extract example) <*> Comprehension (TU $ deconstruct example)
comb = (:*:) <$> (Comprehension . unite $ Just example) <*> (Comprehension . unite $ Just example)

example_zipper :: Zipper Stack Int
example_zipper = Tap 1 . TU $ empty :^: unite (deconstruct example)

main = void $ comb ->> print
