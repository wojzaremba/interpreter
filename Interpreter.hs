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
sm (ValueDouble a) (ValueDouble b) = ValueBool $ a < b
gr (ValueInt a) (ValueInt b) = ValueBool $ a > b
gr (ValueDouble a) (ValueDouble b) = ValueBool $ a > b
esm (ValueInt a) (ValueInt b) = ValueBool $ a <= b
esm (ValueDouble a) (ValueDouble b) = ValueBool $ a <= b
egr (ValueInt a) (ValueInt b) = ValueBool $ a >= b
egr (ValueDouble a) (ValueDouble b) = ValueBool $ a >= b
instance Num Value where
  (ValueInt a) + (ValueInt b) = ValueInt $ a + b
  (ValueDouble a) + (ValueDouble b) = ValueDouble $ a + b
  (ValueInt a) - (ValueInt b) = ValueInt $ a - b
  (ValueDouble a) - (ValueDouble b) = ValueDouble $ a - b
  (ValueInt 0) - (ValueDouble b) = ValueDouble $ - b
  (ValueInt a) * (ValueInt b) = ValueInt $ a * b
  (ValueDouble a) * (ValueDouble b) = ValueDouble $ a * b
  fromInteger a = ValueInt a
  abs (ValueInt a) = ValueInt $ abs a
  abs (ValueDouble a) = ValueDouble $ abs a
  signum (ValueInt a) = ValueInt $ signum a
  signum (ValueDouble a) = ValueDouble $ signum a

fst_ (a, _, _) = a
snd_ (_, a, _) = a
thr_ (_, _, a) = a

getOutput :: Env -> String
getOutput (Env _ _ _ _ output) = join $ reverse output

getSource :: Env -> Source
getSource (Env _ source _ _ _) = source

getFuncArgs :: Func -> [String]
getFuncArgs f = case f of 
                  FuncDecl _ _ arg _ -> Prelude.map (\x -> case x of ArgDecl _ (Ident a) -> a) arg

fresh :: Env -> Env
fresh (Env l s f v o) = Env l s (f+1) v o

freshVal :: Env -> Int
freshVal (Env l s f v o) = f

setVal :: String -> Value -> Res Value
setVal name val =
  do
    (Env l s f v output) <- get
    if (member name l) then 
        do
          let values = insert (head $ l ! name) val v
          put $ Env l s f values output
          return val
      else
        throwError $ "Error during setting variable " ++ name ++ " this variable is not initialized"

initVal :: String -> Value -> Res Value
initVal name val =
  do
    (Env l s f v output) <- get
    let values = insert (f+1) val v
    put $ Env (insert name [(f+1)] l) s (f+1) values output
    return val

delVal :: String -> Env -> Res Value
delVal name (Env l s f v o) =
  if ((member name l) && (Prelude.null (l ! name)) == False) then  
      do
        put $ Env (insert name (tail (l ! name)) l) s f v o
        return ValueVoid
    else
      throwError $ "Error removing from stack variable " ++ name ++ " stack is empty"


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
            throwError $ "Cant find variable " ++ show a
      EInt i -> return $ ValueInt i
      EDouble i -> return $ ValueDouble i
      EStr s -> return $ ValueString s
      ETrue -> return $ ValueBool True
      EFalse -> return $ ValueBool False 
      EEq a b -> simExp (\x y -> ValueBool (x == y))  a b
      ENEq a b -> simExp (\x y -> ValueBool (x /= y))  a b
      ESm a b -> simExp sm a b 
      EGr a b -> simExp gr a b 
      EESm a b -> simExp esm a b 
      EEGr a b -> simExp egr a b 
      EAdd a b -> simExp (+) a b
      ENot a -> do; ValueBool v <- expRes a;return $ ValueBool $ not v
      EAnd a b -> 
        do
          ValueBool v1 <- expRes a
          if (v1 == False) then
              do;return $ ValueBool False
            else
              do
                ValueBool v2 <- expRes b
                if (v2 == False) then do;return $ ValueBool False; else do;return $ ValueBool True
      EOr a b -> 
        do
          ValueBool v1 <- expRes a
          if (v1 == True) then
              do;return $ ValueBool True
            else
              do
                ValueBool v2 <- expRes b
                if (v2 == True) then do;return $ ValueBool True; else do;return $ ValueBool False
      ESub a b -> simExp (-) a b
      EMul a b -> simExp (*) a b
      EMinus a -> simExp (-) (EInt 0) a
      EPlus a -> expRes a
      EToInt a -> 
        do
          v1 <- expRes a
          case v1 of 
            ValueBool v2 -> if (v2) 
                              then return $ ValueInt 1
                              else return $ ValueInt 0
      ECall (Ident a) args -> 
        do
          let (env3, arg, error) = foldl (\a f -> 
                              case (runState (runErrorT (expRes f)) (fst_ a)) of 
                                (Right res, env2) -> (env2, res:(snd_ a), thr_ a) 
                                (Left error, env2) -> (env2, [], error ++ " " ++ (thr_ a))
                            ) (env, [], "") args
          if (error == "") then do;put env3;call a arg; else throwError error
 

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
      IDecl ty ((IdentEmpty (Ident id)):t) -> do; (initVal id (ValueInt 0)); block ((IDecl ty t):h)
      IDecl ty ((IdentExp (Ident id) exp):t) -> do; v <- expRes exp;(initVal id v); block ((IDecl ty t):h)
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
      ValueString h -> printString h
      ValueVoid -> throwError $ "can't printout ValueVoid " ++ (show h)
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
        if (member funcId (getSource env)) then
            do
              let func = (getSource env) ! funcId
              let (FuncDecl _ _ _ (IBlock instr)) = func
              let funcArgs = getFuncArgs func
              let val = zip args funcArgs
              let env2 = foldl (\a f -> let (v, name) = f in snd (runState (runErrorT (initVal name v)) a)) env val   
              let res = runState (runErrorT (block instr)) env2
              case res of
                (Right val, env3) -> do;put $ foldl (\a name -> snd (runState (runErrorT (delVal name a)) a)) env3 funcArgs;return val
                (Left err, env3) -> throwError $ err
          else
            throwError $ "Function " ++ funcId ++ " doesn't exist" 

interpret :: Prog -> (Either String Value, Env)
interpret e = 
  let mFuncs = 
             case e of 
               Entry funcs -> 
                 foldl (\a f -> 
                   case f of
                     FuncDecl _ (Ident id) _ _ -> insert id f a
                 ) empty funcs
  in 
  runState (runErrorT (call "main" [])) (Env empty mFuncs 0 empty [])

run :: String -> (String, Integer)
run s = case pProg (myLexer s) of
    Bad err -> ("Bad" ++ err, -1)
    Ok e -> case interpret e of
              (Right value, env) -> ("value :\n" ++ (show value) ++ "\n\nenv :\n" ++ show env ++ "\n\noutput:\n" ++ (getOutput env), 0)
              (Left (error), env) -> ("error :\n" ++ (show error) ++ "\n\nenv :\n" ++ show env ++ "\n", -1)

main = do
  code <- getContents
  let (out, ret) = run code
  putStr $ out
  if (ret == 0) 
    then exitWith $ ExitSuccess
    else exitWith $ ExitFailure $ fromIntegral ret
  
