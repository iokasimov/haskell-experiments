module Main where

-- Source: http://blog.sigfpe.com/2009/07/monad-for-combinatorial-search-with.html

import Data.Char
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List

data PList a = PList { unO :: [[a]] }
	deriving (Show, Eq)

headm :: Monoid m => [m] -> m
headm (a:as) = a
headm [] = mempty

tailm :: Monoid m => [m] -> [m]
tailm (a:as) = as
tailm [] = []

zipm :: Monoid m => [[m]] -> [m]
zipm ms | all null ms = []
zipm ms = let
	heads = headm <$> ms
	tails = tailm <$> ms
	h = mconcat heads
	t = zipm $ filter (not . null) tails
	in h : t

penalty :: PList ()
penalty = PList [[],[()]]

instance Functor PList where
	fmap f (PList x) = PList $ (fmap . fmap) f x

instance Applicative PList where
	pure x = PList [[x]]

instance Monad PList where
	return x = PList [[x]]
	x >>= f = let PList xs = unO . f <$> x in PList $ join xs where

		join [] = []
		join (m:ms) = let
			part1 = zipm m
			part2 = join ms
			in headm part1 : zipm [tailm part1,part2]

instance Alternative PList where
	empty = PList []
	PList xs <|> PList ys = PList $ zipm [xs,ys]

newtype Parser a = Parser (String -> PList (a, String))

parse :: Parser a -> String -> PList (a, String)
parse (Parser f) x = f x

instance Functor Parser where
	fmap f (Parser g) = Parser (\i -> (\(x, rest) -> (f x, rest)) <$> g i)

instance Applicative Parser where

instance Monad Parser where
	return a = Parser $ \cs -> PList [[(a,cs)]]
	p >>= f = Parser $ \cs -> do
		(a,cs') <- parse p cs
		parse (f a) cs'

instance Alternative Parser where
	empty = Parser $ \_ -> empty
	p <|> q = Parser $ \i -> parse p i <|> parse q i

item :: Parser Char
item = Parser $ \cs -> case cs of
	"" -> empty
	(c:cs) -> PList [[(c,cs)]]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
	c <- item
	if p c then return c else empty

char :: Char -> Parser Char
char c = satisfy (c ==)

lowers = "1234567890-=/"
uppers = "!@#$%^&*()_+?"
lower x = lookup x $ zip uppers lowers
upper x = lookup x $ zip lowers uppers

upperChar x = case upper x of
	Just y -> char y >> return x
	Nothing -> empty

lowerChar x = case lower x of
	Just y -> char y >> return x
	Nothing -> empty

avoid :: Parser ()
avoid = Parser $ \i -> penalty >> return ((), i)

-- keyChar :: Parser Char
keyChar x = char x <|> (avoid >> upperChar x) <|> (avoid >> lowerChar x)

digit = do
	x <- foldl (<|>) empty (keyChar <$> "0123456789")
	return (fromIntegral (ord x-ord '0'))

number1 :: Integer -> Parser Integer
number1 m = return m <|> do
	n <- digit
	number1 $ 10 * m + n

number :: Parser Integer
number = digit >>= number1

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = chainl1 p op <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do {a <- p; rest a <|> return a } where

	rest a = do
		f <- op
		b <- p
		rest $ f a b

shouldHave c = keyChar c <|> (avoid >> return c)

expr = term `chainl1` addop
term = monomial `chainl1` mulop
monomial = factor `chainl1` powop
factor = number <|> do {shouldHave '('; n <- expr; shouldHave ')'; return n}
powop = keyChar '^' >> return (^)
addop = do {keyChar '+'; return (+)} <|> do {keyChar '-'; return (-)}
mulop = do {keyChar '*'; return (*)} <|> do {keyChar '/'; return (div)}

end :: Parser ()
end = Parser $ \i -> if null i then PList [[((),"")]] else empty

completeExpr = do
	n <- expr
	end
	return n

ex2 = parse completeExpr "2^(1+3"

main = print ex2 -- parse (keyChar '0') ")"
