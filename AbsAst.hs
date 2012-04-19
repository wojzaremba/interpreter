module AbsAst where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq,Ord,Show)
data Type =
   Int
 | Bool
 | Double
 | Void
  deriving (Eq,Ord,Show)

data Arg =
   ArgDecl Ident Type
  deriving (Eq,Ord,Show)

data Prog =
   Entry [Func]
  deriving (Eq,Ord,Show)

data Func =
   FuncDecl Type Ident [Arg] Instr
  deriving (Eq,Ord,Show)

data Instr =
   IBlock [Instr]
 | IDeclSt Type Ident Exp
 | IDecl Type [Ident]
 | IRet Exp
 | IRetEmpty
 | IExp Exp
 | IIf Exp Instr
 | IIfElse Exp Instr Instr
 | IWhile Exp Instr
  deriving (Eq,Ord,Show)

data Exp =
   EVarSet Ident Exp
 | EOr Exp Exp
 | EAnd Exp Exp
 | EEq Exp Exp
 | ENEq Exp Exp
 | ESm Exp Exp
 | EGr Exp Exp
 | EESm Exp Exp
 | EEGr Exp Exp
 | EAdd Exp Exp
 | ESub Exp Exp
 | EMul Exp Exp
 | EDiv Exp Exp
 | EMod Exp Exp
 | ENot Exp
 | EPlus Exp
 | EMinus Exp
 | EVar Ident
 | ECall Ident [Exp]
 | EDouble Double
 | EInt Integer
 | ETrue
 | EFalse
 | EStr String
 | EToInt Exp
  deriving (Eq,Ord,Show)

