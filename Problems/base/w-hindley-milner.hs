module Main where

-- Source: https://github.com/wh5a/Algorithm-W-Step-By-Step

import "containers" Data.Map
import "containers" Data.Set
import "mtl" Control.Monad.Error
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "pretty" Text.PrettyPrint

data Expression
	= Variable String
	| Literal Literal
	| Application Expression Expression
	| Abstraction String Expression
	| Let String Expression Expression
	deriving (Eq, Ord, Show)

data Literal
	= Integer Integer
	| Boolean Bool
	deriving (Eq, Ord, Show)

data Grounded
	= Integer'
	| Boolean'
	deriving (Eq, Ord, Show)

data Monotype
	= Grounded Grounded
	| Parametric String
	| Function Monotype Monotype
	deriving (Eq, Ord, Show)

data Polytype = Polytype [String] Monotype

-- Finite mappings from type variables to types
type Substitution = Map String Monotype

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Data.Map.union (apply s1 <$> s2) s1

class Types a where
	ftv :: a -> Set String
	apply :: Substitution -> a -> a

instance Types Monotype where
	ftv (Parametric n) = Data.Set.singleton n
	ftv (Grounded Integer') = mempty
	ftv (Grounded Boolean') = mempty
	ftv (Function st tt) = Data.Set.union (ftv st) (ftv tt)
	apply s (Function st tt) = Function (apply s st) (apply s tt)
	apply _ t = t

instance Types Polytype where
	ftv (Polytype vars t) = Data.Set.difference (ftv t) $ Data.Set.fromList vars
	apply s (Polytype vars t) = Polytype vars $ apply (Prelude.foldr Data.Map.delete s vars) t

instance Types a => Types [a] where
	ftv l = Prelude.foldr Data.Set.union mempty $ ftv <$> l

newtype Г = Г (Map String Polytype)

remove :: Г -> String -> Г
remove (Г env) var = Г $ Data.Map.delete var env

instance Types Г where
	ftv (Г env) = ftv $ Data.Map.elems env
	apply s (Г env) = Г $ apply s <$> env

generalize :: Г -> Monotype -> Polytype
generalize env t = Polytype vars t where

	vars :: [String]
	vars = Data.Set.toList $ Data.Set.difference (ftv t) (ftv env)

data TIEnv = TIEnv

type TIState = Int

type TI a = ErrorT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = runStateT (runReaderT (runErrorT t) TIEnv) 0

newTyVar :: String -> TI Monotype
newTyVar prefix = do
	modify (+1)
	Parametric . (prefix ++) . show <$> get

instantiate :: Polytype -> TI Monotype
instantiate (Polytype vars t) = do
	nvars <- mapM (\_ -> newTyVar "a") vars
	let s = Data.Map.fromList $ zip vars nvars
	return $ apply s t

mgu :: Monotype -> Monotype -> TI Substitution
mgu (Function src tgt) (Function src' tgt') = do
	s1 <- mgu src src'
	s2 <- mgu (apply s1 tgt) (apply s1 tgt')
	pure $ composeSubst s1 s2
mgu (Parametric u) t = varBind u t
mgu t (Parametric u) = varBind u t
mgu (Grounded Integer') (Grounded Integer') = pure mempty
mgu (Grounded Boolean') (Grounded Boolean') = pure mempty
mgu t t' = throwError $ "Monotypes do not unify: " ++ show t ++ " vs. " ++ show t'

varBind :: String -> Monotype -> TI Substitution
varBind u t | t == Parametric u = pure mempty
			| u `Data.Set.member` ftv t = throwError $ "occurs check fails: " ++ u ++ " vs. " ++ show t
			| otherwise = pure $ Data.Map.singleton u t

tiLit :: Literal -> TI (Substitution, Monotype)
tiLit (Integer _) = return (mempty, Grounded Integer')
tiLit (Boolean _) = return (mempty, Grounded Boolean')

ti :: Г -> Expression -> TI (Substitution, Monotype)
ti (Г env) (Variable n) = case Data.Map.lookup n env of
	Nothing -> throwError $ "Unbound variable: " ++ n
	Just sigma -> (,) mempty <$> instantiate sigma
ti _ (Literal l) = tiLit l
ti env (Abstraction n e) = do
	tv <- newTyVar "a"
	let Г env' = remove env n
	let env'' = Г $ Data.Map.union env' $ Data.Map.singleton n $ Polytype [] tv
	(s1, t1) <- ti env'' e
	pure (s1, Function (apply s1 tv) t1)
ti env exp@(Application e1 e2) = do
	tv <- newTyVar "a"
	(s1, t1) <- ti env e1
	(s2, t2) <- ti (apply s1 env) e2
	s3 <- mgu (apply s2 t1) (Function t2 tv)
	pure (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
	`catchError` (\e -> throwError $ e ++ "\n in " ++ show exp)
ti env (Let x e1 e2) = do
	(s1, t1) <- ti env e1
	let Г env' = remove env x
	let t' = generalize (apply s1 env) t1
	let env'' = Г $ Data.Map.insert x t' env'
	(s2, t2) <- ti (apply s1 env'') e2
	pure (s1 `composeSubst` s2, t2)

typeInference :: Map String Polytype -> Expression -> TI Monotype
typeInference env e = uncurry apply <$> ti (Г env) e

test :: Expression -> IO ()
test e = fst <$> runTI (typeInference mempty e) >>= \case
	Left err -> putStrLn $ show e ++ "\n " ++ err ++ "\n"
	Right t -> putStrLn $ show e ++ " : " ++ show t ++ "\n"

-- let id = \x -> x in id
e0 = Let "id" (Abstraction "x" (Variable "x")) (Variable "id")

main = test e0
