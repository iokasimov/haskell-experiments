import "base" Data.Char (Char)
import "base" System.IO (print)
import "base" Text.Show (Show (show))

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances
import Gears.Utils (empty_list)

data Variable = A' | B'

deriving instance Show Variable

algae :: Variable -> Comprehension Maybe Variable
algae A' = item @Push A' . item @Push B' ! Comprehension empty_list
algae B' = item @Push A' ! Comprehension empty_list

--------------------------------------------------------------------------------

start :: Comprehension Maybe Variable
start = item @Push A' ! Comprehension empty_list

main = print . run ! algae =<< algae =<< algae =<< algae =<< start
