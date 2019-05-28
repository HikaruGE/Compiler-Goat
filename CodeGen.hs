module CodeGen where

import GoatAST
import SymbolTable
-- import Analyze
import qualified Data.Map as Map

type Reg = Int
type LabelNum = Int

type LocalState = (CallTable, VarTable, Ident, LabelNum)

data BinOpClass 
    = Arithmetic | Comparision | Logic


-- compile :: Program -> String
-- compile ast 
--     = 
--         let t = (Map.empty, Map.empty) in
--             programCode ast t

programCode :: Program -> SymTable -> String
programCode (Program m) t@(c,p)
    =   show(Map.lookup "main" c) ++"\n"++
        show(Map.lookup "omg" c) ++"\n"++
        "    call proc_main\n" ++
        "    halt\n" ++ 
        (procsCode m t)

procsCode :: [Proc] -> SymTable -> String
procsCode procs st = concatMap (procCode st) procs

procCode :: SymTable -> Proc -> String
procCode (callt, proct) (Proc id params decls stmts)
    =   
        let 
            t@(t1,t2,_,_) = (callt, (getVarTable id proct), id, 0)  -- Todo
        in
            show(Map.lookup "x1" t2) ++"\n"++
            show(Map.lookup "x2" t2) ++"\n"++
            show(Map.lookup "x3" t2) ++"\n"++
            "\n"++"\n"++
            (procLabel id) ++
            "    push_stack_frame 1\n" ++
            "#decl\n" ++
            (stmtsCode stmts t) ++
            "    pop_stack_frame 1\n" ++
            "    return\n"

-- procCode :: Proc -> VarTable -> String
-- procCode (Proc id x y z) t
--   = (procLabel id) ++ (prolog n) ++ (paramsCode x t 0) ++ (declsCode y t m) ++ (stmtsCode z t) ++ (epilog n)
--     where n = getSize t
--           m = length x

prolog :: Int -> String
prolog n = "    push_stack_frame " ++ (show n) ++ "\n"

epilog :: Int -> String
epilog n = "    pop_stack_frame " ++ (show n) ++ 
           "\n    return\n"
          
paramsCode :: [Param] -> LocalState -> Int -> String
paramsCode [] _ _
    = ""
paramsCode (x:xs) t n
    = (paramCode x t n) ++ (paramsCode xs t (n + 1))

paramCode :: Param -> LocalState -> Int -> String
paramCode _ _ n
    = "    store " ++ s ++ ", r" ++ s ++ "\n"
    where s = show n

blockLabel :: String -> String
blockLabel s
    = "label_" ++ s ++ ":\n"

procLabel :: String -> String
procLabel s
    = "proc_" ++ s ++ ":\n"

stmtsCode :: [Stmt] -> LocalState -> String
stmtsCode stmts gt = concatMap (stmtCode gt) stmts

stmtCode :: LocalState -> Stmt -> String
stmtCode gt (Write expr) = 
    let 
        (code, reg, ty) = exprCode expr 0
    in
        code ++ 
        "    call_builtin " ++ (writeBuiltin ty) ++ "\n"



exprCode :: Expr -> Reg -> (String, Reg, BaseType)
exprCode (BoolConst b) r = ("    int_const" ++ regToStr(r) ++ ", "++ show(val) ++"\n",
                                r,
                                BoolType)
                            where 
                                val = boolToInt b
exprCode (IntConst i) r = ("    int_const" ++ regToStr(r) ++ ", "++ show(i) ++"\n",
                                r,
                                IntType)
exprCode (FloatConst f) r = ("    real_const" ++ regToStr(r) ++ ", "++ show(f) ++"\n",
                                r,
                                FloatType)
exprCode (StrConst s) r = ("    string_const" ++ regToStr(r) ++ ", \""++ s ++"\"\n",
                                r,
                                StrType)

exprCode (Unary unaOp expr) r = 
    let
        (code, reg, ty) = exprCode expr r
        code' = 
            case unaOp of
                Neg -> "    not" ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
                Minus -> "    neg_" ++ (ozTy ty) ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
    in
        (code++code', reg, ty)

exprCode (Binary binOp lexpr rexpr) r =
    let
        left@(lcode, lreg, lty) = exprCode lexpr r
        right@(rcode, rreg, rty) = exprCode rexpr (r+1)
        (isConvert, convertCode, commonTy) = tyConvert left right
        (code', afterType) = binOpCode binOp lreg rreg commonTy
    in
        (lcode ++ rcode ++ convertCode ++ code', lreg, afterType)

binOpCode :: BinOp -> Reg -> Reg -> BaseType -> (String, BaseType)
binOpCode binOp reg1 reg2 commonTy =
    case (classifyBinOp binOp) of
        Arithmetic -> 
            case binOp of 
                Add -> (concat(["    add_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        commonTy)
                Sub -> (concat(["    sub_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        commonTy)
                Mul -> (concat(["    mul_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        commonTy)
                Div -> (concat(["    div_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        commonTy)
        Comparision -> 
            case binOp of
                Eq -> (concat(["    cmp_eq_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        BoolType)
                NotEq -> (concat(["    cmp_ne_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        BoolType)
                Lt -> (concat(["    cmp_lt_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        BoolType)
                LtEq -> (concat(["    cmp_le_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        BoolType)
                Gt -> (concat(["    cmp_gt_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        BoolType)
                GtEq -> (concat(["    cmp_ge_", (ozTy commonTy),regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        BoolType)
        Logic ->
            case binOp of
                And -> (concat(["    and",regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        BoolType)
                Or -> (concat(["    or",regToStr(reg1),",",regToStr(reg1),",",regToStr(reg2),"\n"]),
                        BoolType)

tyConvert :: (String, Reg, BaseType) -> (String, Reg, BaseType) -> (Bool, String, BaseType)
tyConvert (_,reg1,ty1) (_,reg2,ty2) =
    if (isBothBoolType ty1 ty2) then
        (False, "", BoolType)
    else
        if (isBothSameType ty1 ty2) then
            (False, "", ty1)
        else
            let 
                reg = convertReg (reg1,ty1) (reg2,ty2) 
            in
                (True , "    int_to_real" ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n", FloatType)

isBothBoolType :: BaseType -> BaseType -> Bool
isBothBoolType BoolType BoolType = True
isBothBoolType _ _ = False

isBothSameType :: BaseType -> BaseType -> Bool
isBothSameType ty1 ty2 = ty1 == ty2 

convertReg :: (Reg, BaseType) -> (Reg, BaseType) -> Reg
-- convertReg (reg1,ty1) (reg2,ty2) = 1
convertReg (reg1,IntType) (reg2,FloatType) = reg1
convertReg (reg1,FloatType) (reg2,IntType) = reg2

ozTy :: BaseType -> String
ozTy IntType = "int"
ozTy FloatType = "real"
ozTy BoolType = "int"

boolToInt :: Bool -> Int
boolToInt True = 1 
boolToInt False = 0

writeOpr :: Expr -> ProcTable -> String
writeOpr (BoolConst b) _ = "string_const"
writeOpr (IntConst b) _ = "int_const"
writeOpr (FloatConst b) _ = "real_const"
writeOpr (StrConst b) _ = "string_const"

writeBuiltin :: BaseType -> String 
writeBuiltin ty = case ty of
                BoolType -> "print_bool"
                IntType -> "print_int"
                FloatType -> "print_real"
                StrType -> "print_string"

regToStr :: Int -> String
regToStr i = " r" ++ show(i)

classifyBinOp :: BinOp -> BinOpClass
classifyBinOp binOp = 
    case binOp of
        Add -> Arithmetic
        Sub -> Arithmetic
        Mul -> Arithmetic
        Div -> Arithmetic
        Eq -> Comparision
        NotEq -> Comparision
        Lt -> Comparision
        LtEq -> Comparision
        Gt -> Comparision
        GtEq -> Comparision
        And -> Logic
        Or -> Logic


