module CodeGen where

import GoatAST
import SymbolTable
-- import Analyze
import qualified Data.Map as Map

type Reg = Int
type LabelNum = Int

type LocalState = (CallTable, VarTable, Ident, LabelNum)
incLabel :: LocalState -> LocalState
incLabel (ct, vt, id, l) = (ct, vt, id, (l+1))

data BinOpClass 
    = Arithmetic | Comparision | Logic

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
            -- show(Map.lookup "x1" t2) ++"\n"++
            -- show(Map.lookup "x2" t2) ++"\n"++
            -- show(Map.lookup "x3" t2) ++"\n"++
            -- "\n"++"\n"++
            (procLabel id) ++
            (prolog t) ++
            (declsCode decls t) ++
            fst(stmtsCode stmts t) ++
            (epilog t)

declsCode :: [Decl] -> LocalState -> String
declsCode decls state = concatMap (declCode state) decls

declCode :: LocalState -> Decl -> String
declCode (_, vt, _, _) decl =
    case decl of
        DeclVar ty varid ->
            let
                initDecl = defaultValueCode ty
                slotNum = getSlotNum(lookupVarTable varid vt)
                store = defaultStoreCode slotNum
            in
                initDecl ++ store

        DeclArray ty varid lenth ->
            let
                initDecl = defaultValueCode ty
                slotNum = getSlotNum(lookupVarTable varid vt)
                store = arrayDeclCode slotNum lenth
            in
                initDecl ++ store
        
        DeclMatrix ty varid len1 len2 ->
            let
                initDecl = defaultValueCode ty
                slotNum = getSlotNum(lookupVarTable varid vt)
                store = arrayDeclCode slotNum (len1*len2)
            in
                initDecl ++ store

defaultValueCode :: BaseType -> String
defaultValueCode BoolType = "    int_const r0, 0\n"
defaultValueCode IntType = "    int_const r0, 0\n"
defaultValueCode FloatType = "    real_const r0, 0.0\n"

defaultStoreCode :: SlotNum -> String
defaultStoreCode slotNum = "    store " ++ show(slotNum) ++ ", r0\n"

arrayDeclCode :: Int -> Int -> String -- start slot, length
arrayDeclCode slot 0 = ""
arrayDeclCode slot len = (defaultStoreCode slot) ++ (arrayDeclCode (slot+1) (len-1))

-- stmtsCode :: [Stmt] -> LocalState -> String
-- stmtsCode stmts gt = concatMap (stmtCode gt) stmts

-- procCode :: Proc -> VarTable -> String
-- procCode (Proc id x y z) t
--   = (procLabel id) ++ (prolog n) ++ (paramsCode x t 0) ++ (declsCode y t m) ++ (stmtsCode z t) ++ (epilog n)
--     where n = getSize t
--           m = length x



prolog :: LocalState -> String
prolog (_, vt, _, _) = "    push_stack_frame " ++ (show (getSize vt)) ++ "\n"

epilog :: LocalState -> String
epilog (_, vt, _, _) = "    pop_stack_frame " ++ (show (getSize vt)) ++ "\n    return\n"
          
-- paramsCode :: [Param] -> LocalState -> Int -> String
-- paramsCode [] _ _
--     = ""
-- paramsCode (x:xs) t n
--     = (paramCode x t n) ++ (paramsCode xs t (n + 1))

-- paramCode :: Param -> LocalState -> Int -> String
-- paramCode _ _ n
--     = "    store " ++ s ++ ", r" ++ s ++ "\n"
--     where s = show n

blockLabel :: Ident -> String -> String
blockLabel id s
    = id ++ "_label_" ++ s ++ ":\n"

procLabel :: String -> String
procLabel s
    = "proc_" ++ s ++ ":\n"

-- Code generator for a list of statements
stmtsCode :: [Stmt] -> LocalState -> (String, LocalState)
stmtsCode [] state 
    = ("",state)
stmtsCode (x:xs) state = 
    let
        (code, newstate) = stmtCode x state
        (code',newstate1) = stmtsCode xs newstate
    in
        (code ++ code',
        newstate1)



-- stmtsCode :: [Stmt] -> LocalState -> String
-- stmtsCode stmts gt = concatMap (stmtCode gt) stmts

stmtCode :: Stmt -> LocalState -> (String, LocalState)
stmtCode (Assign var rExpr) state@(_, varTable, _, _) =
    case var of
        (Id varid) -> 
            let
                varInfo = lookupVarTable varid varTable
                (code, r, ty) = exprCode rExpr 0 state
            in
                (code ++
                 (storeSingleVar varid varInfo r),
                    state)

        (Array varid lExpr) -> 
            let
                r0 = 0
                r1 = 1
                varInfo = lookupVarTable varid varTable
                (code1, r1', ty1) = exprCode lExpr r1 state
                code2 = "    load_address" ++ regToStr(r0) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n"
                code3 = "    sub_offset" ++ regToStr(r0) ++ "," ++ regToStr(r0) ++ "," ++ regToStr(r1') ++ "\n"
                (code4, r1'', ty2) = exprCode rExpr r1 state
                code5 = "    store_indirect" ++ regToStr(r0) ++ "," ++ regToStr(r1) ++ "\n"
            in
                (concat([code1,code2,code3,code4,code5]),
                    state
                )
                
        (Matrix varid lExpr1 lExpr2) ->
            let
                varInfo = lookupVarTable varid varTable
                (code1, r1, ty1) = exprCode lExpr1 2 state
                (code2, r2, ty2) = exprCode lExpr2 4 state
                (d1, d2) = getDim varInfo
                (code0, _, rTy) = exprCode rExpr 0 state
            in
                (code0 ++
                "    load_address" ++ regToStr(1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                code1 ++ code2 ++
                "    int_const r3, " ++ show(d1) ++ "\n" ++
                "    mul_int r2, r2, r3\n" ++
                "    add_int r2, r2, r4\n" ++
                "    sub_offset" ++ regToStr(1) ++ "," ++ regToStr(1) ++ "," ++ regToStr(2) ++ "\n" ++
                "    store_indirect" ++ regToStr(1) ++ "," ++ regToStr(0) ++ "\n"
                ,state)

stmtCode (Read var) state@(_, varTable, _, _) =
    case var of
        (Id varid) -> 
            let
                varInfo = lookupVarTable varid varTable
            in
                ("    call_builtin read_" ++ readOzTy(getBaseType varInfo) ++ "\n" ++
                 (storeSingleVar varid varInfo 0)
                    ,state)
        
        (Array varid expr) -> 
            let
                varInfo = lookupVarTable varid varTable
                (code, r, ty) = exprCode expr 2 state
            in
                ("    call_builtin read_" ++ readOzTy(getBaseType varInfo) ++ "\n" ++
                 "    load_address" ++ regToStr(1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                 code ++
                 "    sub_offset" ++ regToStr(1) ++ "," ++ regToStr(1) ++ "," ++ regToStr(r) ++ "\n" ++
                 "    store_indirect" ++ regToStr(1) ++ "," ++ regToStr(0) ++ "\n"
                    ,state)

        (Matrix varid expr1 expr2) ->
            let
                varInfo = lookupVarTable varid varTable
                (code1, r1, ty1) = exprCode expr1 2 state
                (code2, r2, ty2) = exprCode expr2 4 state
                (d1, d2) = getDim varInfo
            in
                ("    call_builtin read_" ++ readOzTy(getBaseType varInfo) ++ "\n" ++
                 "    load_address" ++ regToStr(1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                 code1 ++ code2 ++
                 "    int_const r3, " ++ show(d1) ++ "\n" ++
                 "    mul_int r2, r2, r3\n" ++
                 "    add_int r2, r2, r4\n" ++
                 "    sub_offset" ++ regToStr(1) ++ "," ++ regToStr(1) ++ "," ++ regToStr(2) ++ "\n" ++
                 "    store_indirect" ++ regToStr(1) ++ "," ++ regToStr(0) ++ "\n"
                    ,state)

stmtCode (Write expr) state = 
    let 
        (code, reg, ty) = exprCode expr 0 state
    in
        (code ++ 
        "    call_builtin " ++ (writeBuiltin ty) ++ "\n",
        state)

stmtCode (If expr stmts) state@(_,_,id,l) = 
    let
        (code, _, _) = exprCode expr 0 state
        newState1@(_,_,_,l1) = incLabel state
        newState2@(_,_,_,l2) = incLabel newState1
        (innerStmtsCode,newState3) = stmtsCode stmts newState2
    in
        (code ++ 
        "    branch_on_true r0, " ++ id ++ "_label_" ++ (show l) ++ "\n" ++ 
        "    branch_uncond "++ id ++"_label_" ++ show(l1) ++ "\n" ++
        id ++ "_label_" ++ (show l) ++ ":\n" ++
            innerStmtsCode ++ 
        (blockLabel id (show l1)),
            newState3
        )

stmtCode (IfElse expr stmts1 stmts2) state@(_,_,id,l) =
    let
        (code, _, _) = exprCode expr 0 state
        newState@(_,_,_,l1) = incLabel state
        newState1@(_,_,_,l2) = incLabel newState
        (innerStmtsCode1,newState2) = stmtsCode stmts1 newState1
        (innerStmtsCode2,newState3) = stmtsCode stmts1 newState2
    in
        (code++
        "    branch_on_false r0, " ++ id ++ "_label_" ++ (show l) ++ "\n" ++ 
            innerStmtsCode1 ++ 
        "    branch_uncond "++ id ++"_label_" ++ show(l1) ++ "\n" ++
        (blockLabel id (show l)) ++
            innerStmtsCode2 ++
        (blockLabel id (show l1))
        ,
        newState2
        )

stmtCode (While expr stmts) state@(_,_,id,l) =
    let
        (code, _, _) = exprCode expr 0 state
        newState1@(_,_,_,l1) = incLabel state
        newState2@(_,_,_,l2) = incLabel newState1
        newState3@(_,_,_,l3) = incLabel newState2
        (innerStmtsCode, newState4) = stmtsCode stmts newState3
    in
        ((blockLabel id (show l)) ++ 
            code ++
        "    branch_on_true r0, " ++ id ++ "_label_" ++ show(l1) ++ "\n" ++ 
        "    branch_uncond " ++ id ++ "_label_" ++ show(l2) ++ "\n" ++ 
        (blockLabel id (show l1)) ++
            innerStmtsCode ++
        "    branch_uncond " ++ id ++ "_label_" ++ show(l) ++ "\n" ++ 
        (blockLabel id (show l2)),
        newState4
        )

storeSingleVar :: Ident -> VarInfo -> Reg -> String
storeSingleVar varid varInfo r =
    "    store " ++ show(getSlotNum (varInfo)) ++ "," ++ regToStr(0) ++ "\n" 

exprCode :: Expr -> Reg -> LocalState -> (String, Reg, BaseType)
exprCode (BoolConst b) r _ = 
    ("    int_const" ++ regToStr(r) ++ ", "++ show(val) ++"\n",
        r,
        BoolType)
    where 
        val = boolToInt b
exprCode (IntConst i) r _ = 
    ("    int_const" ++ regToStr(r) ++ ", "++ show(i) ++"\n",
        r,
        IntType)
exprCode (FloatConst f) r _ = 
    ("    real_const" ++ regToStr(r) ++ ", "++ show(f) ++"\n",
        r,
        FloatType)
exprCode (StrConst s) r _ = 
    ("    string_const" ++ regToStr(r) ++ ", \""++ s ++"\"\n",
        r,
        StrType)

exprCode (Var var) r state@(_, varTable, _, _) =
    case var of
        (Id varid) -> 
            let
                varInfo = lookupVarTable varid varTable
            in
                ("    load" ++ regToStr(r) ++ ", " ++ show((getSlotNum varInfo)) ++ "\n",
                    r,
                    (getBaseType varInfo))
        (Array varid expr) -> 
            let
                varInfo = lookupVarTable varid varTable
                (code1, r1, ty) = exprCode expr r state
                code2 = "    load_address" ++ regToStr(r1+1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n"
                code3 = "    sub_offset" ++ regToStr(r1) ++ "," ++ regToStr(r1+1) ++ "," ++ regToStr(r) ++ "\n"
                code4 = "    load_indirect" ++ regToStr(r1) ++ "," ++ regToStr(r1) ++ "\n"
            in
                (concat([code1,code2,code3,code4]),
                    r,
                    (getBaseType varInfo)
                )
        (Matrix varid expr1 expr2) -> 
            let
                varInfo = lookupVarTable varid varTable
                (code1, r1, ty1) = exprCode expr1 (r+2) state
                (code2, r2, ty2) = exprCode expr2 (r+4) state
                (d1, d2) = getDim varInfo
            in
                (
                 ("    load_address" ++ regToStr(r+1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                 code1 ++ code2 ++
                 "    int_const " ++ regToStr(r+3) ++ ", " ++ show(d1) ++ "\n" ++
                 "    mul_int " ++ regToStr(r+2) ++ ", " ++ regToStr(r+2) ++ ", " ++ regToStr(r+3) ++ "\n" ++
                 "    add_int " ++ regToStr(r+2) ++ ", " ++ regToStr(r+2) ++ ", " ++ regToStr(r+4) ++ "\n" ++
                 "    sub_offset" ++ regToStr(r+1) ++ "," ++ regToStr(r+1) ++ "," ++ regToStr(r+2) ++ "\n" ++
                 "    load_indirect" ++ regToStr(r) ++ "," ++ regToStr(r+1) ++ "\n"),
                 r,
                 (getBaseType varInfo)
                )

exprCode (Unary unaOp expr) r state = 
    let
        (code, reg, ty) = exprCode expr r state
        code' = 
            case unaOp of
                Neg -> "    not" ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
                Minus -> "    neg_" ++ (ozTy ty) ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
    in
        (code++code', reg, ty)

exprCode (Binary binOp lexpr rexpr) r state =
    let
        left@(lcode, lreg, lty) = exprCode lexpr r state
        right@(rcode, rreg, rty) = exprCode rexpr (r+1) state
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

readOzTy :: BaseType -> String
readOzTy IntType = "int"
readOzTy FloatType = "real"
readOzTy BoolType = "bool"

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


