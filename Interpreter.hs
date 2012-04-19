import LexAst
import ParAst
import AbsAst
import ErrM
import Data.Map as Map
import System

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error


type Res = ErrorT String (State Env)
type Source = Map String Func
type Loc = Map String [Int]
type Fresh = Int
type Output = [String]
data Env = Env Loc Source Fresh Values Output deriving Show
type Values = Map Int Value 
data Value = ValueInt Integer | ValueDouble Double | ValueBool Bool | ValueString String | ValueVoid deriving (Show, Eq)

sm (ValueInt a) (ValueInt b) = ValueBool $ a < b
gr (ValueInt a) (ValueInt b) = ValueBool $ a > b
esm (ValueInt a) (ValueInt b) = ValueBool $ a <= b
egr (ValueInt a) (ValueInt b) = ValueBool $ a >= b
instance Num Value where
  (ValueInt a) + (ValueInt b) = ValueInt $ a + b
  (ValueInt a) * (ValueInt b) = ValueInt $ a * b
  fromInteger a = ValueInt a
  abs (ValueInt a) = ValueInt $ abs a
  signum (ValueInt a) = ValueInt $ signum a


getOutput :: Env -> String
getOutput (Env _ _ _ _ output) = join $ reverse output

getSource :: Env -> Source
getSource (Env _ source _ _ _) = source

getFuncArgs :: Func -> [String]
getFuncArgs f = case f of 
                  FuncDecl _ _ arg _ -> Prelude.map (\x -> case x of ArgDecl (Ident a) _ -> a) arg

fresh :: Env -> Env
fresh (Env l s f v o) = Env l s (f+1) v o

freshVal :: Env -> Int
freshVal (Env l s f v o) = f

setVal :: String -> Value -> Res Value
setVal name val =
  do
    (Env l s f v output) <- get
    let loc =
            if (member name l)
              then insert name ((f+1):(l ! name)) l
              else insert name [(f+1)] empty     
    let values = insert (f+1) val v
    put $ Env loc s (f+1) values output
    return val

delVal :: String -> Env -> Env
delVal name (Env l s f v o) =
  Env (insert name (tail (l ! name)) l) s f v o


simExp :: (Value -> Value -> Value) -> Exp -> Exp -> Res Value
simExp f a b =
  do    
    v1 <- expRes a    
    v2 <- expRes b
    return $ f v1 v2


expRes :: Exp -> Res Value
expRes e = 
  do    
    env <- get
    let Env loc source fresh values output = env
    case e of
      EVarSet (Ident a) exp -> 
        do 
          v1 <- expRes exp
          setVal a v1
      EVar (Ident a) -> 
        if (member a loc)
          then return $ values ! (head (loc ! a))
          else 
            --error "Cant find variable " ++ show a
            return ValueVoid
      EInt i -> return $ ValueInt i
      EStr s -> return $ ValueString s
      ETrue -> return $ ValueBool True
      EFalse -> return $ ValueBool False 
      EEq a b -> simExp (\x y -> ValueBool (x == y))  a b
      ESm a b -> simExp sm a b 
      EGr a b -> simExp gr a b 
      EESm a b -> simExp esm a b 
      EEGr a b -> simExp egr a b 
      EAdd a b -> simExp (+) a b
      EMul a b -> simExp (*) a b
      EMinus a -> simExp (-) (EInt 0) a
      EToInt a -> 
        do
          v1 <- expRes a
          case v1 of 
            ValueBool v2 -> if (v2) 
                              then return $ ValueInt 1
                              else return $ ValueInt 0
      ECall (Ident a) args -> 
--        let (env3, arg) = foldl (\a f -> let (res, env2) = (fst a) >>= runState (expRes f) (fst a) in (env2, res:snd a) ) (env, []) args
        do
          return []
          env <- get
          foldl (\a f -> do; ) env args
--          let (Right env_) = env
--          let (env3, arg) = foldl (\a f -> let (res, env2) = runState (expRes f) (fst a) in (env2, res:snd a) ) (env, []) args
--          put env3
          call a arg

block :: [Instr] -> Res Value
block [] = 
  do 
    env <- get
    return ValueVoid
block (t:h) = 
  do 
    env@(Env loc source fresh values output) <- get
    case t of 
      IBlock instr -> do; block instr; block h
      IDecl _ [] -> block h
      IDecl ty ((Ident id):t) -> do; (setVal id (ValueInt 0)); block ((IDecl ty t):h)
      IDeclSt _ (Ident id) exp -> do; v <- expRes exp;setVal id v; block h
      IIf exp instr ->
        do 
          ValueBool a <- expRes exp
          if (a) then block (instr:h) else block h
      IIfElse exp instr instr2-> do; ValueBool a <- expRes exp; if (a) then block (instr:h) else block (instr2:h)
      IRet exp -> expRes exp
      IRetEmpty -> return ValueVoid
      IExp exp -> do; expRes exp; block h
      w@(IWhile exp instr) -> 
        do 
          ValueBool a <- expRes exp
          if (a) then block (instr:w:h) else block h
      

tell :: Env -> String -> Env
tell (Env loc source fresh values output) str = Env loc source fresh values (str:output)

printString :: String -> Res Value
printString arg =
  do
    env@(Env loc source fresh values output) <- get
    put $ (Env loc source fresh values ((arg++"\n"):output))
    return ValueVoid

printValue :: [Value] -> Res Value
printValue [] = return ValueVoid
printValue (h:t) = 
  do
    case h of
      ValueInt h -> printString (show h)
      ValueDouble h -> printString (show h)
      ValueBool h -> printString (show h)
      ValueString h -> printString (show h)
    printValue t

call :: String -> [Value] -> Res Value
call funcId args = 
  case funcId of
    "printInt" -> printValue args
    "printDouble" -> printValue args
    "printString" -> printValue args
    "printBool" -> printValue args
    _ -> 
      do
        env <- get     
        let func = (getSource env) ! funcId
        let (FuncDecl _ _ _ (IBlock instr)) = func
        let funcArgs = getFuncArgs func
        let val = zip args funcArgs
        let env2 = foldl (\a f -> let (v, name) = f in setVal name v a) env val   
        let (val, env3) = runState (block instr) env2 
        put $ foldl (\a name -> delVal name a) env3 funcArgs    
        return val

interpret :: Prog -> (Either String Value, Env)
interpret e = 
  let mFuncs = 
             case e of 
               Entry funcs -> 
                 foldl (\a f -> 
                   case f of
                     FuncDecl _ (Ident id) _ _ -> insert id f a
                 ) empty funcs
  in runErrorT (call "main" []) (Env empty mFuncs 0 empty [])
--type Res = ErrorT String (State Env)
--runErrorT :: ErrorT e m a -> m (Either e a)

run :: String -> (String, Integer)
run s = case pProg (myLexer s) of
    Bad err -> ("Bad" ++ err, -1)
 --   Ok e -> let (ValueInt value, env) = interpret e in ("value :\n" ++ (show value) ++ "\n\nenv :\n" ++ show env ++ "\n", value)
    Ok e -> let (ValueInt value, env) = interpret e in (getOutput env, value)

main = do
  putStr "aa"
  code <- getContents
  let (out, ret) = run code
  putStr $ out
  if (ret == 0) 
    then exitWith $ ExitSuccess
    else exitWith $ ExitFailure $ fromIntegral ret
  
