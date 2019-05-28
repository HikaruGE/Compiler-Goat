module GoatAST where

-----------------------------------
-- Specification of an AST for Goat
-----------------------------------

type Ident = String

-- Data type for a Goat program.
data Program
  = Program [Proc]
    deriving (Show, Eq)

-- Data type for a procedure.
data Proc
  = Proc Ident [Param] [Decl] [Stmt]
    deriving (Show, Eq)

-- Data type for a formal parameter in the header of a procedure.
data Param
  = Param Indic BaseType Ident
    deriving (Show, Eq)

-- Data type for a declaration.
data Decl
  = DeclVar BaseType Ident
  | DeclArray BaseType Ident Int
  | DeclMatrix BaseType Ident Int Int
    deriving (Show, Eq)

-- Data type for a statement.
data Stmt
  = Assign Var Expr
  | Read Var
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

-- Data type for an expression.
data Expr
  = BoolConst Bool
  | IntConst Int
  | FloatConst Float
  | StrConst String
  | Var Var
  | Unary UnaOp Expr
  | Binary BinOp Expr Expr
    deriving (Show, Eq)

-- Data type for a binary operator.
data BinOp
  = Add | Sub | Mul | Div
  | And | Or
  | Eq | NotEq
  | Lt | LtEq
  | Gt | GtEq
    deriving (Show, Eq)

-- Data type for a unary operator.
data UnaOp
  = Neg
  | Minus
    deriving (Show, Eq)

-- Data type for a parameter passing indicator.
data Indic
  = Val | Ref | Loc
    deriving (Show, Eq)

-- Data type for a base type.
data BaseType
  = BoolType | IntType | FloatType | StrType
    deriving (Show, Eq)


-- Data type for a variable.
data Var
  = Id Ident
  | Array Ident Expr
  | Matrix Ident Expr Expr
    deriving (Show, Eq)
