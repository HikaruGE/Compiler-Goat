module PrettyPrinter where

import GoatAST

-- The main function which takes an input of type Program and returns a well-formed string.
-- Each pair of consecutive procedures are separated by a single blank line.
-- There is only one newline character after the last procedure, instead of a blank line.
pPrint :: Program -> String
pPrint (Program [])
  = ""
pPrint (Program [x])
  = procPrint x
pPrint (Program (x:y:ys))
  = (procPrint x) ++ "\n" ++ (pPrint (Program (y:ys)))

-- Function which transforms a procedure into a string.
procPrint :: Proc -> String
procPrint (Proc a b c d)
  = (headerPrint a b) ++ (foldl indent "" (map declPrint c)) ++ "begin\n" ++ (foldl (++) "" (map (stmtPrint 4) d)) ++ "end\n"

-- Default indentation which is four white spaces.
indent :: String -> String -> String
indent a b
  = a ++ "    " ++ b

-- Function which transforms the header part into a string.
headerPrint :: Ident -> [Param] -> String
headerPrint a b
  = "proc" ++ " " ++ a ++ " " ++ "(" ++ (paramListPrint b) ++ ")\n"

-- Function which transforms the parameter list in header into a string.
paramListPrint :: [Param] -> String
paramListPrint []
  = ""
paramListPrint [x]
  = paramPrint x
paramListPrint (x:y:ys)
  = (paramPrint x) ++ ", " ++ (paramListPrint (y:ys))

-- Function which transforms a single parameter into a string.
paramPrint :: Param -> String
paramPrint (Param a b c)
  = (indicPrint a) ++ " " ++ (baseTypePrint b) ++ " " ++ c

-- Function which transforms a parameter passing indicator into a string.
indicPrint :: Indic -> String
indicPrint Val
  = "val"
indicPrint Ref
  = "ref"

-- Function which transforms a base type into a string.
baseTypePrint :: BaseType -> String
baseTypePrint BoolType
  = "bool"
baseTypePrint IntType
  = "int"
baseTypePrint FloatType
  = "float"

-- Function which transforms a declaration into a string.
declPrint :: Decl -> String
declPrint (DeclVar a b)
  = (baseTypePrint a) ++ " " ++ b ++ ";\n"
declPrint (DeclArray a b c)
  = (baseTypePrint a) ++ " " ++ b ++ "[" ++ (show c) ++ "];\n"
declPrint (DeclMatrix a b c d)
  = (baseTypePrint a) ++ " " ++ b ++ "[" ++ (show c) ++ ", " ++ (show d) ++ "];\n"

-- Function which transforms a statement into a string.
stmtPrint :: Int -> Stmt -> String
stmtPrint n (Assign a b)
  = (replicate n ' ') ++ (varPrint a) ++ " := " ++ (exprPrint False b) ++ ";\n"
stmtPrint n (Read a)
  = (replicate n ' ') ++ "read " ++ (varPrint a) ++ ";\n"
stmtPrint n (Write a)
  = (replicate n ' ') ++ "write " ++ (exprPrint False a) ++ ";\n"
stmtPrint n (Call a b)
  = (replicate n ' ') ++ "call " ++ a ++ "(" ++ (exprListPrint b) ++ ");\n"
stmtPrint n (If a b)
  = (replicate n ' ') ++ "if " ++ (exprPrint False a) ++ " then\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) b)) ++ (replicate n ' ') ++ "fi\n"
stmtPrint n (IfElse a b c)
  = (replicate n ' ') ++ "if " ++ (exprPrint False a) ++ " then\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) b)) ++ (replicate n ' ') ++ "else\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) c)) ++ (replicate n ' ') ++ "fi\n"
stmtPrint n (While a b)
  = (replicate n ' ') ++ "while " ++ (exprPrint False a) ++ " do\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) b)) ++ (replicate n ' ') ++ "od\n"

-- Function which transforms a variable into a string.
varPrint :: Var -> String
varPrint (Id a)
  = a
varPrint (Array a b)
  = a ++ "[" ++ (exprPrint False b) ++ "]"
varPrint (Matrix a b c)
  = a ++ "[" ++ (exprPrint False b) ++ ", " ++ (exprPrint False c) ++ "]"

-- Function which transforms an expression list into a string.
exprListPrint :: [Expr] -> String
exprListPrint []
  = ""
exprListPrint [x]
  = exprPrint False x
exprListPrint (x:y:ys)
  = (exprPrint False x) ++ ", " ++ (exprListPrint (y:ys))

-- Function which transforms a single expression into a string.
-- In order to print well-formed parentheses, a boolean flag is maintained.
-- False represents being at the top level, or outermost position of an expression, in which case parentheses are not needed.
-- True represents being at any low level, or inner position of an expression, in which case parentheses might be needed.
-- Notice about unary operators negataion(!) and minus(-):
-- Although not mentioned in the specification, we adopt the convention that !true and !false are not parenthesized in any case,
-- which means well-formed output might look like: !true || !false.
-- And -x, where x stands for an arbitrary expression, is not parenthesized if it appears as the top level of an expression,
-- or as the first operand of a binary operation, which means well-formed output might look like: -1, -(1 * 2), -1 + (-2 * 3).
exprPrint :: Bool -> Expr -> String
exprPrint _ (BoolConst True)
  = "true"
exprPrint _ (BoolConst False)
  = "false"
exprPrint _ (IntConst a)
  = show a
exprPrint _ (FloatConst a)
  = show a
exprPrint _ (StrConst a)
  = "\"" ++ a ++ "\""
exprPrint _ (Var a)
  = varPrint a
exprPrint False (Unary a b)
  = (unaPrint a) ++ (exprPrint True b)
exprPrint True (Unary Neg a)
  = (unaPrint Neg) ++ (exprPrint True a)
exprPrint True (Unary Minus a)
  = "(" ++ (unaPrint Minus) ++ (exprPrint True a) ++ ")"
exprPrint False (Binary a (Unary b c) d)
  = (exprPrint False (Unary b c)) ++ (binPrint a) ++ (exprPrint True d)
exprPrint True (Binary a (Unary b c) d)
  = "(" ++ (exprPrint False (Unary b c)) ++ (binPrint a) ++ (exprPrint True d) ++ ")"
exprPrint False (Binary a b c)
  = (exprPrint True b) ++ (binPrint a) ++ (exprPrint True c)
exprPrint True (Binary a b c)
  = "(" ++ (exprPrint True b) ++ (binPrint a) ++ (exprPrint True c) ++ ")"

-- Function which transforms a unary operator into a string.
unaPrint :: UnaOp -> String
unaPrint Neg
  = "!"
unaPrint Minus
  = "-"

-- Function which transform a binary operator into a string.
binPrint :: BinOp -> String
binPrint Add
  = " + "
binPrint Sub
  = " - "
binPrint Mul
  = " * "
binPrint Div
  = " / "
binPrint And
  = " && "
binPrint Or
  = " || "
binPrint Eq
  = " = "
binPrint NotEq
  = " != "
binPrint Lt
  = " < "
binPrint LtEq
  = " <= "
binPrint Gt
  = " > "
binPrint GtEq
  = " >= "
