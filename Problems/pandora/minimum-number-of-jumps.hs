module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO
import "base" Data.Int (Int)
import "base" System.IO (print)

import Gears.Instances ()

-- jump :: Nonempty List Int -> _
-- jump capabilities = insert (extract capabilities) <$>

-- possible 0 capabilities = empty
-- possible n capabilities = capabilities .-+

--------------------------------------------------------------------------------

example :: Nonempty List Int
example = 6 += 2 += 4 += 0 += 5 += 1 += 1 += 4 += 2 += point 9

main = void $ do
	print "WIP"
	-- delete (equate 4) example ->> print
	-- view (sub @(Delete First)) example 4 ->> print
	-- print "-------------------------------------"
	-- view (sub @(Delete All)) example 4 ->> print
