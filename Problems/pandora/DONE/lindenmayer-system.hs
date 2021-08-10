import "base" Data.Char (Char)
import "base" System.IO (print)
import "base" Text.Show (Show (show))

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances

data Variable = A' | B'

deriving instance Show Variable

algae :: Variable -> Comprehension Maybe Variable
algae A' = item @Push A' $ item @Push B' empty
algae B' = item @Push A' empty

--------------------------------------------------------------------------------

start :: Comprehension Maybe Variable
start = item @Push A' empty

main = print . run $ algae =<< algae =<< algae =<< algae =<< start
