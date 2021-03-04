module Main where

-- Source: https://github.com/wh5a/Algorithm-W-Step-By-Step

import "base" Data.Traversable
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
	deriving (Eq, Ord)

instance Show Expression where
	show (Variable v) = show v
	show (Literal l) = show l
	show (Application e e') = show e ++ "" ++ show e'
	show (Abstraction arg body) = "\\" ++ show arg ++ " -> " ++ show body
	show (Let v e body) = "let " ++ show v ++ " " ++ show e ++ " in " ++ show body

data Literal
	= Integer Integer
	| Boolean Bool
	deriving (Eq, Ord)

instance Show Literal where
	show (Integer i) = show i
	show (Boolean b) = show b

data Grounded
	= Integer'
	| Boolean'
	deriving (Eq, Ord, Show)

data Monotype
	= Parametric Int
	| Grounded Grounded
	| Function Monotype Monotype
	deriving (Eq, Ord, Show)

data Polytype = Polytype [Int] Monotype

-- Finite mappings from type variables to types
type Substitution = Map Int Monotype

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Data.Map.union (apply s1 <$> s2) s1

class Types a where
	ftv :: a -> Set Int
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

	vars :: [Int]
	vars = Data.Set.toList $ Data.Set.difference (ftv t) (ftv env)

data TIEnv = TIEnv

type TIState = Int

type TI a = ErrorT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = runStateT (runReaderT (runErrorT t) TIEnv) 0

newTyVar :: TI Monotype
newTyVar = modify (+1) *> (Parametric <$> get)

instantiate :: Polytype -> TI Monotype
instantiate (Polytype vars t) = flip apply t . Data.Map.fromList . zip vars <$> for vars (const newTyVar)

mgu :: Monotype -> Monotype -> TI Substitution
mgu (Function src tgt) (Function src' tgt') = do
	arg <- mgu src src'
	result <- mgu (apply arg tgt) (apply arg tgt')
	pure $ composeSubst arg result
mgu (Parametric u) t = varBind u t
mgu t (Parametric u) = varBind u t
mgu (Grounded Integer') (Grounded Integer') = pure mempty
mgu (Grounded Boolean') (Grounded Boolean') = pure mempty
mgu t t' = throwError $ "Monotypes do not unify: " ++ show t ++ " vs. " ++ show t'

varBind :: Int -> Monotype -> TI Substitution
varBind n t | t == Parametric n = pure mempty
			| n `Data.Set.member` ftv t = throwError $ "occurs check fails: " ++ show n ++ " vs. " ++ show t
			| otherwise = pure $ Data.Map.singleton n t

ti :: Г -> Expression -> TI (Substitution, Monotype)
ti (Г env) (Variable n) = case Data.Map.lookup n env of
	Nothing -> throwError $ "Unbound variable: " ++ n
	Just sigma -> (,) mempty <$> instantiate sigma
ti _ (Literal (Integer _)) = pure (mempty, Grounded Integer')
ti _ (Literal (Boolean _)) = pure (mempty, Grounded Boolean')
ti env (Abstraction n e) = do
	tv <- newTyVar
	let Г env' = remove env n
	let env'' = Г $ Data.Map.union env' $ Data.Map.singleton n $ Polytype [] tv
	(s1, t1) <- ti env'' e
	pure (s1, Function (apply s1 tv) t1)
ti env exp@(Application e1 e2) = do
	tv <- newTyVar
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

e0 = Let "id" (Abstraction "x" (Variable "x")) (Variable "id")
e1 = Let "id" (Abstraction "x" (Variable "x")) (Application (Variable "id") (Variable "id"))

main = for [e0, e1] test
