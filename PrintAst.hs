module PrintAst where

-- pretty-printer generated by the BNF converter

import AbsAst
import Char

-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString i)



instance Print Type where
  prt i e = case e of
   Int  -> prPrec i 0 (concatD [doc (showString "int")])
   Bool  -> prPrec i 0 (concatD [doc (showString "boolean")])
   Double  -> prPrec i 0 (concatD [doc (showString "double")])
   Void  -> prPrec i 0 (concatD [doc (showString "void")])


instance Print Arg where
  prt i e = case e of
   ArgDecl type' id -> prPrec i 0 (concatD [prt 0 type' , prt 0 id])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Prog where
  prt i e = case e of
   Entry funcs -> prPrec i 0 (concatD [prt 0 funcs])


instance Print Func where
  prt i e = case e of
   FuncDecl type' id args instr -> prPrec i 0 (concatD [prt 0 type' , prt 0 id , doc (showString "(") , prt 0 args , doc (showString ")") , prt 0 instr])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Instr where
  prt i e = case e of
   IBlock instrs -> prPrec i 0 (concatD [doc (showString "{") , prt 0 instrs , doc (showString "}")])
   IDecl type' identexps -> prPrec i 0 (concatD [prt 0 type' , prt 0 identexps , doc (showString ";")])
   IRet exp -> prPrec i 0 (concatD [doc (showString "return") , prt 0 exp , doc (showString ";")])
   IRetEmpty  -> prPrec i 0 (concatD [doc (showString "return") , doc (showString ";")])
   IExp exp -> prPrec i 0 (concatD [prt 0 exp , doc (showString ";")])
   IIf exp instr -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 instr])
   IIfElse exp instr0 instr -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 instr0 , doc (showString "else") , prt 0 instr])
   IWhile exp instr -> prPrec i 0 (concatD [doc (showString "while") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 instr])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print IdentExp where
  prt i e = case e of
   IdentEmpty id -> prPrec i 0 (concatD [prt 0 id])
   IdentExp id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 exp])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Exp where
  prt i e = case e of
   EVarSet id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 exp])
   EOr exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , doc (showString "||") , prt 2 exp])
   EAnd exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "&&") , prt 3 exp])
   EEq exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "==") , prt 4 exp])
   ENEq exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "!=") , prt 4 exp])
   ESm exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString "<") , prt 5 exp])
   EGr exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString ">") , prt 5 exp])
   EESm exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString "<=") , prt 5 exp])
   EEGr exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString ">=") , prt 5 exp])
   EAdd exp0 exp -> prPrec i 5 (concatD [prt 5 exp0 , doc (showString "+") , prt 6 exp])
   ESub exp0 exp -> prPrec i 5 (concatD [prt 5 exp0 , doc (showString "-") , prt 6 exp])
   EMul exp0 exp -> prPrec i 6 (concatD [prt 6 exp0 , doc (showString "*") , prt 7 exp])
   EDiv exp0 exp -> prPrec i 6 (concatD [prt 6 exp0 , doc (showString "/") , prt 7 exp])
   EMod exp0 exp -> prPrec i 6 (concatD [prt 6 exp0 , doc (showString "%") , prt 7 exp])
   ENot exp -> prPrec i 7 (concatD [doc (showString "!") , prt 7 exp])
   EPlus exp -> prPrec i 7 (concatD [doc (showString "+") , prt 7 exp])
   EMinus exp -> prPrec i 7 (concatD [doc (showString "-") , prt 7 exp])
   EVar id -> prPrec i 7 (concatD [prt 0 id])
   ECall id exps -> prPrec i 7 (concatD [prt 0 id , doc (showString "(") , prt 0 exps , doc (showString ")")])
   EDouble d -> prPrec i 7 (concatD [prt 0 d])
   EInt n -> prPrec i 7 (concatD [prt 0 n])
   ETrue  -> prPrec i 7 (concatD [doc (showString "true")])
   EFalse  -> prPrec i 7 (concatD [doc (showString "false")])
   EStr str -> prPrec i 7 (concatD [prt 0 str])
   EToInt exp -> prPrec i 7 (concatD [doc (showString "(") , doc (showString "int") , doc (showString ")") , prt 7 exp])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])


