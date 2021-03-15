module Main where

-- Source: https://dodisturb.me/posts/2018-12-25-The-Essence-of-Datalog.html

import "base" Data.Function (fix)
import "base" Data.List (nub)
import "base" Data.Maybe (mapMaybe)

data Rule = Rule { _head :: Atom, _body :: [ Atom ] }

data Atom = Atom { _predSym :: String, _terms :: [ Term ] }
	deriving (Eq, Show)

data Term = Var String | Sym String
	deriving (Eq, Show)

resolve :: Substitution -> Term -> Term
resolve _ (Sym symbol) = Sym symbol
resolve substitution (Var var) =
	maybe (Var var) id $ lookup (Var var) substitution

type Program = [Rule]

type KnowledgeBase = [Atom]

type Substitution = [(Term, Term)]

substitute :: Atom -> Substitution -> Atom
substitute atom substitution = atom { _terms = resolve substitution <$> _terms atom } where

unify :: Atom -> Atom -> Maybe Substitution
unify (Atom predSym ts) (Atom predSym' ts') = if predSym == predSym' then go $ zip ts ts' else Nothing where

	go :: [(Term, Term)] -> Maybe Substitution
	go [] = Just []
	go ((Sym s, Sym s') : rest) = if s == s' then go rest else Nothing
	go ((Var v, Sym s) : rest) = do
		incompleteSubstitution <- go rest
		case (Var v) `lookup` incompleteSubstitution of
			Just s' | Sym s /= s' -> Nothing
			_ -> pure $ (Var v, Sym s) : incompleteSubstitution
	go ((_, Var _) : _) = error "The second atom is assumed to be ground."

evalAtom :: KnowledgeBase -> Atom -> [Substitution] -> [Substitution]
evalAtom kb atom substitutions = do
	substitution <- substitutions
	let downToEarthAtom = substitute atom substitution
	extension <- mapMaybe (unify downToEarthAtom) kb
	pure $ substitution <> extension

walk :: KnowledgeBase -> [Atom] -> [Substitution]
walk kb = foldr (evalAtom kb) [[]]

evalRule :: KnowledgeBase -> Rule -> KnowledgeBase
evalRule kb (Rule head body) = substitute head <$> walk kb body

immediateConsequence :: Program -> KnowledgeBase -> KnowledgeBase
immediateConsequence rules kb = nub . (kb <>) . concatMap (evalRule kb) $ rules

solve :: Program -> KnowledgeBase
solve rules = if all isRangeRestricted rules then fix step []
	else error "The input program is not range-restricted." where

	step :: (KnowledgeBase -> KnowledgeBase) -> (KnowledgeBase -> KnowledgeBase)
	step f currentKB | nextKB <- immediateConsequence rules currentKB =
		if nextKB == currentKB then currentKB else f nextKB

isRangeRestricted :: Rule -> Bool
isRangeRestricted rule = vars (_head rule) `isSubsetOf` concatMap vars (_body rule) where

	isSubsetOf as bs = all (`elem` bs) as

	vars atom = nub $ filter (\case {Var _ -> True; _ -> False}) $ _terms atom

{-
- airline("Moscow", "London").
- airline("London", "Athens").

- path(X,Y) :- airline(X,Y).
- path(X,Z) :- airline(X,Y), airline(Y,Z).
-}

facts :: Program
facts = (\terms -> Rule (Atom "airline" terms) []) <$>
	[[ Sym "Moscow", Sym "London"], [ Sym "London", Sym "Athens"]]

rules :: Program
rules =
	[ Rule (Atom "path" [Var "X", Var "Y"]) [Atom "airline" [Var "X", Var "Y"]]
	, Rule (Atom "path" [Var "X", Var "Z"]) [Atom "airline" [Var "X", Var "Y"], Atom "airline" [Var "Y", Var "Z"]]
	]

queries :: Program
queries = [Rule (Atom "query1" [ Var "Intermediate" ]) $ Atom "path" <$>
	[[Sym "Moscow", Var "Intermediate"], [Var "Intermediate", Sym "Athens"]]]

query :: String -> Program -> [Substitution]
query predSym pr = case queryVarsL of
	[queryVars] -> zip queryVars <$> relevantKnowledgeBaseSyms
	[] -> error $ "The query '" <> predSym <> "' doesn't exist."
	_  -> error $ "The query '" <> predSym <> "' has multiple clauses."
	where

		relevantKnowledgeBase = filter ((== predSym) . _predSym) $ solve pr
		relevantKnowledgeBaseSyms = _terms <$> relevantKnowledgeBase

		queryRules = filter ((== predSym) . _predSym . _head) pr
		queryVarsL = _terms . _head <$> queryRules

main = print $ query "query1" (facts <> rules <> queries)
