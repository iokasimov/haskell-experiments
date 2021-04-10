import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO
import "base" Data.Char (Char)
import "base" System.IO (print)

type Identifier = Nonempty List Char

data Binding a = Variable Identifier | Abstraction Identifier a

-- type ABT a = Construction (Conclusion (Binding a)) AST

main = print "typechecked"
