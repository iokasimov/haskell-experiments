import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)

import Gears.Instances

-- enumerate :: Nonempty Stack Int ->
-- enumerate elements = extract elements <$> deconstruct elements

--------------------------------------------------------------------------------

listify :: [a] -> Stack a -> [a]
listify r (TU (Just (Construct x next))) = listify (x : r) $ TU next
listify r (TU Nothing) = r

-- deriving instance (Show a, Show b) => Show (a :*: b)
-- deriving instance Show a => Show (Maybe a)

--------------------------------------------------------------------------------

example :: Nonempty Stack Int
example = insert 1 $ insert 2 $ insert 3 $ point 4

comb = (:*:) <$> point (extract example) <*> Comprehension (TU $ deconstruct example)

main = do
	void $ deconstruct example ->>> print
	void $ comb ->> print
