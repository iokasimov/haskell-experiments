module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO
import "base" Data.Int (Int)
import "base" System.IO (print)

-- jump :: Nonempty Stack Int -> _
-- jump capabilities = insert (extract capabilities) <$>

-- possible 0 capabilities = empty
-- possible n capabilities = capabilities .-+

--------------------------------------------------------------------------------

example :: Nonempty Stack Int
example = insert 6 $ insert 2 $ insert 4 $ insert 0 $ insert 5
	$ insert 1 $ insert 1 $ insert 4 $ insert 2 $ point 9

main = void $ example ->> print
