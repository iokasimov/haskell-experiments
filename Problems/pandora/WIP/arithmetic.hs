import "base" Data.Int (Int)
import "base" System.IO (print)
import "base" Text.Show (Show)
import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances

data Token = Digit Number | Parentheses Position | Negative | Positive

tokenize :: ASCII -> Maybe Token
tokenize (Number number) = Just $ Digit number
tokenize (Sign (Bracket pos Round)) = Just $ Parentheses pos
tokenize (Sign Minus) = Just Negative
tokenize (Sign Plus) = Just Positive
tokenize _ = Nothing

-- type Parser token struct result =
	-- struct token -> Conclusion (struct token) result

data Expression = Int Int
	| Add Expression Expression
	| Subtract Expression Expression

evaluate :: Expression -> Int
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Subtract x y) = evaluate x - evaluate y
evaluate (Int i) = i

-- opened_bracket :: Parser Sign ()

-- -1 + (2 + 3)
-- symbols_example :: List ASCII
-- symbols_example = item @Push # Sign Minus
-- 	$ item @Push # Number N1
-- 	$ item @Push # Sign Plus
-- 	$ item @Push # Sign (Bracket Opened Round)
-- 	$ item @Push # Number N2
-- 	$ item @Push # Sign Plus
-- 	$ item @Push # Number N3
-- 	$ item @Push # Sign (Bracket Closed Round)
-- 	$ empty

-- -1 + (2 + 3)
expression_example :: Expression
expression_example = Subtract (Add (Int 2) (Int 3)) (Int 1)

deriving instance Show ASCII
deriving instance Show Sign
deriving instance Show Number
deriving instance Show Control
deriving instance Show Case
deriving instance Show Quote
deriving instance Show Slash
deriving instance Show Letter
deriving instance Show Position
deriving instance Show Bracket
deriving instance Show Token

main = void $ do
	print $ evaluate expression_example
	-- tokens_example ->> print
	-- tokens_example ->> print . tokenize
