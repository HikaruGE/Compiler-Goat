-- Team Lihai

module CodeGen where

import GoatAST
import SymbolTable
import qualified Data.Map as Map

type Reg = Int
type LabelNum = Int

type LocalState = (CallTable, VarTable, Ident, LabelNum)
incLabel :: LocalState -> LocalState
incLabel (ct, vt, id, l) = (ct, vt, id, (l+1))

data BinOpClass 
    = Arithmetic | Comparision | Logic
        deriving(Eq)

programCode :: Program -> SymTable -> String
programCode (Program m) t@(c,p)   
    =   if (Map.member "main" c) then
            if length (getCallList "main" c) == 0 then
                "    call proc_main\n" ++
                "    halt\n" ++ 
                (procsCode m t)
            else
                error("ERROR: Non-Zero Arity for Main Procedure")
        else
            error("ERROR: No Main Procedure")
            

procsCode :: [Proc] -> SymTable -> String
procsCode procs st = concatMap (procCode st) procs

procCode :: SymTable -> Proc -> String
procCode (callt, proct) (Proc id params decls stmts)
    =   
        let 
            t@(t1,t2,_,_) = (callt, (getVarTable id proct), id, 0)  -- Todo
        in
            (procLabel id) ++
            (prolog t) ++
            (pass params 0) ++
            (declsCode decls t) ++
            fst(stmtsCode stmts t) ++
            (epilog t)

pass :: [Param] -> Int -> String
pass [] _
    = ""
pass (x:xs) n
    = "    store " ++ (show n) ++ ", r" ++ (show n) ++ "\n" ++ (pass xs (n + 1))

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

prolog :: LocalState -> String
prolog (_, vt, _, _) = "    push_stack_frame " ++ (show (getSize vt)) ++ "\n"

epilog :: LocalState -> String
epilog (_, vt, _, _) = "    pop_stack_frame " ++ (show (getSize vt)) ++ "\n    return\n"

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


stmtCode :: Stmt -> LocalState -> (String, LocalState)
stmtCode (Assign var rExpr) state@(_, varTable, _, _) =
    case var of
        (Id varid) -> 
            let
                varInfo = lookupVarTable varid varTable
                (code, r, ty) = exprCode rExpr 0 state
                indic = getIndic varInfo
                varTy = getBaseType varInfo
            in
                if varTy == ty then
                    if indic /= Ref then
                        (code ++
                        (storeSingleVar varid varInfo r),
                            state)
                    else
                        (code ++
                        "    load" ++ regToStr(1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                        "    store_indirect" ++ regToStr(1) ++ "," ++ regToStr(0) ++ "\n",
                            state)
                else if (varTy == FloatType && ty == IntType) then
                    if indic /= Ref then
                        (code ++
                        "int_to_real r0, r0\n" ++
                        (storeSingleVar varid varInfo r),
                            state)
                    else
                        (code ++
                        "    load" ++ regToStr(1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                        "    store_indirect" ++ regToStr(1) ++ "," ++ regToStr(0) ++ "\n",
                            state)
                else
                    error ("ERROR: Incompatible Types for Assignment\n")

        (Array varid lExpr) -> 
            let
                r0 = 0
                r1 = 1
                varInfo = lookupVarTable varid varTable
                varTy = getBaseType varInfo
                (code1, r1', ty1) = exprCode lExpr r1 state
                code2 = "    load_address" ++ regToStr(r0) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n"
                code3 = "    sub_offset" ++ regToStr(r0) ++ "," ++ regToStr(r0) ++ "," ++ regToStr(r1') ++ "\n"
                (code4, r1'', ty2) = exprCode rExpr r1 state
                code5 = "    store_indirect" ++ regToStr(r0) ++ "," ++ regToStr(r1) ++ "\n"
            in
                if ty1 == IntType then
                    if varTy == ty2 || (varTy == FloatType && ty2 == IntType) then
                        (concat([code1,code2,code3,code4,code5]),
                            state
                        )
                    else
                        error ("ERROR: Incompatible Types for Assignment\n")
                else
                    error ("ERROR: Invalid Index")
                
        (Matrix varid lExpr1 lExpr2) ->
            let
                varInfo = lookupVarTable varid varTable
                varTy = getBaseType varInfo
                (code1, r1, ty1) = exprCode lExpr1 2 state
                (code2, r2, ty2) = exprCode lExpr2 4 state
                (d1, d2) = getDim varInfo
                (code0, _, rTy) = exprCode rExpr 0 state
            in
                if ty1 == IntType && ty2 == IntType then
                    if varTy == rTy || (varTy == FloatType && rTy == IntType) then
                        (code0 ++
                        "    load_address" ++ regToStr(1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                        code1 ++ code2 ++
                        "    int_const r3, " ++ show(d1) ++ "\n" ++
                        "    mul_int r2, r2, r3\n" ++
                        "    add_int r2, r2, r4\n" ++
                        "    sub_offset" ++ regToStr(1) ++ "," ++ regToStr(1) ++ "," ++ regToStr(2) ++ "\n" ++
                        "    store_indirect" ++ regToStr(1) ++ "," ++ regToStr(0) ++ "\n"
                        ,state)
                    else
                        error ("ERROR: Incompatible Types for Assignment\n")
                else
                    error ("ERROR: Invalid Indice")
                

stmtCode (Read var) state@(_, varTable, _, _) =
    case var of
        (Id varid) -> 
            let
                varInfo = lookupVarTable varid varTable
                isNotFP = getFpFlag varInfo
                indic = getIndic varInfo
            in
                if (isScalar (Var var) state) then
                    if indic /= Ref then 
                        ("    call_builtin read_" ++ readOzTy(getBaseType varInfo) ++ "\n" ++
                        (storeSingleVar varid varInfo 0)
                            ,state)
                    else
                        ("    call_builtin read_" ++ readOzTy(getBaseType varInfo) ++ "\n" ++
                        "    load_address" ++ regToStr(1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                        "    store_indirect" ++ regToStr(1) ++ "," ++ regToStr(0) ++ "\n"
                            ,state)
                else
                    error ("ERROR: Non-Scalar Argument for Read\n")

        (Array varid expr) -> 
            let
                varInfo = lookupVarTable varid varTable
                (code, r, ty) = exprCode expr 2 state
            in
                if (isScalar (Var var) state) then
                    ("    call_builtin read_" ++ readOzTy(getBaseType varInfo) ++ "\n" ++
                    "    load_address" ++ regToStr(1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                    code ++
                    "    sub_offset" ++ regToStr(1) ++ "," ++ regToStr(1) ++ "," ++ regToStr(r) ++ "\n" ++
                    "    store_indirect" ++ regToStr(1) ++ "," ++ regToStr(0) ++ "\n"
                        ,state)
                else
                    error ("ERROR: Non-Scalar Argument for Read\n")

        (Matrix varid expr1 expr2) ->
            let
                varInfo = lookupVarTable varid varTable
                (code1, r1, ty1) = exprCode expr1 2 state
                (code2, r2, ty2) = exprCode expr2 4 state
                (d1, d2) = getDim varInfo
            in
                if (isScalar (Var var) state) then
                    ("    call_builtin read_" ++ readOzTy(getBaseType varInfo) ++ "\n" ++
                    "    load_address" ++ regToStr(1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                    code1 ++ code2 ++
                    "    int_const r3, " ++ show(d1) ++ "\n" ++
                    "    mul_int r2, r2, r3\n" ++
                    "    add_int r2, r2, r4\n" ++
                    "    sub_offset" ++ regToStr(1) ++ "," ++ regToStr(1) ++ "," ++ regToStr(2) ++ "\n" ++
                    "    store_indirect" ++ regToStr(1) ++ "," ++ regToStr(0) ++ "\n"
                        ,state)
                else
                    error ("ERROR: Non-Scalar Argument for Read\n")

stmtCode (Write expr) state = 
    let 
        (code, reg, ty) = exprCode expr 0 state
    in
        (code ++ 
        "    call_builtin " ++ (writeBuiltin ty) ++ "\n",
        state)

stmtCode (If expr stmts) state@(_,_,id,l) = 
    let
        (code, _, conTy) = exprCode expr 0 state
        newState1@(_,_,_,l1) = incLabel state
        newState2@(_,_,_,l2) = incLabel newState1
        (innerStmtsCode,newState3) = stmtsCode stmts newState2
    in
        if conTy == BoolType then
            (code ++ 
            "    branch_on_true r0, " ++ id ++ "_label_" ++ (show l) ++ "\n" ++ 
            "    branch_uncond "++ id ++"_label_" ++ show(l1) ++ "\n" ++
            id ++ "_label_" ++ (show l) ++ ":\n" ++
                innerStmtsCode ++ 
            (blockLabel id (show l1)),
                newState3
            )
        else
            error ("ERROR: Non-Boolean Condition\n")

stmtCode (IfElse expr stmts1 stmts2) state@(_,_,id,l) =
    let
        (code, _, conTy) = exprCode expr 0 state
        newState@(_,_,_,l1) = incLabel state
        newState1@(_,_,_,l2) = incLabel newState
        (innerStmtsCode1,newState2) = stmtsCode stmts1 newState1
        (innerStmtsCode2,newState3) = stmtsCode stmts2 newState2
    in
        if conTy == BoolType then
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
        else
            error ("ERROR: Non-Boolean Condition\n")

stmtCode (While expr stmts) state@(_,_,id,l) =
    let
        (code, _, conTy) = exprCode expr 0 state
        newState1@(_,_,_,l1) = incLabel state
        newState2@(_,_,_,l2) = incLabel newState1
        newState3@(_,_,_,l3) = incLabel newState2
        (innerStmtsCode, newState4) = stmtsCode stmts newState3
    in
        if conTy == BoolType then
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
        else
            error ("ERROR: Non-Boolean Condition\n")


stmtCode (Call id x) state@(ct,vt,_,_)
    = ((callCode id x (getCallList id ct) 0 state) ++ "    call proc_" ++ id ++ "\n", state)

-- Code for procedure call
callCode :: Ident -> [Expr] -> [Param] -> Int -> LocalState -> String
callCode _ [] (x:xs) _ _
    = error "Error: Parameter number mismatch"
callCode _ (x:xs) [] _ _
    = error "Error: Parameter number mismatch"
callCode _ [] [] _ _
    = ""
callCode id (x:xs) ((Param Val BoolType _):ys) n state
    = case (exprCode x n state) of (s, _, BoolType) -> s ++ (callCode id xs ys (n + 1) state)
                                   (_, _, _) -> error "Error: Parameter type mismatch"
callCode id (x:xs) ((Param Val IntType _):ys) n state
    = case (exprCode x n state) of (s, _, IntType) -> s ++ (callCode id xs ys (n + 1) state)
                                   (_, _, _) -> error "Error: Parameter type mismatch"
callCode id (x:xs) ((Param Val FloatType _):ys) n state
    = case (exprCode x n state) of (s, _, FloatType) -> s ++ (callCode id xs ys (n + 1) state)
                                   (s, _, IntType) -> s ++ "int_to_real r" ++ (show n) ++ ", r" ++ (show n) ++ "\n" ++ (callCode id xs ys (n + 1) state)
                                   (_, _, _) -> error "Error: Parameter type mismatch"

-- Parameter passing for call by reference
-- Detect expressions that are type-correct but not being scalars
callCode id (x:xs) ((Param Ref BoolType _):ys) n state
    = if (isScalar x state) then
          case (exprCodeRef x n state) of (s, _, BoolType) -> s ++ (callCode id xs ys (n + 1) state)
                                          (_, _, _) -> error "Error: Parameter type mismatch"
      else
          error "Error: Non-scalar argument for call-by-reference parameter"
callCode id (x:xs) ((Param Ref IntType _):ys) n state
    = if (isScalar x state) then
          case (exprCodeRef x n state) of (s, _, IntType) -> s ++ (callCode id xs ys (n + 1) state)
                                          (_, _, _) -> error "Error: Parameter type mismatch"
      else
          error "Error: Non-scalar argument for call-by-reference parameter"
callCode id (x:xs) ((Param Ref FloatType _):ys) n state
    = if (isScalar x state) then 
          case (exprCodeRef x n state) of (s, _, FloatType) -> s ++ (callCode id xs ys (n + 1) state)
                                          (s, _, IntType) -> s ++ "int_to_real r" ++ (show n) ++ ", r" ++ (show n) ++ "\n" ++ (callCode id xs ys (n + 1) state)
                                          (_, _, _) -> error "Error: Parameter type mismatch"
      else
          error "Error: Non-scalar argument for call-by-reference parameter"

callCode id (x:xs) ((Param Loc BoolType _):ys) n state
    = case (exprCode x n state) of (s, _, BoolType) -> s ++ (callCode id xs ys (n + 1) state)
                                   (_, _, _) -> error "Error: Parameter type mismatch"
callCode id (x:xs) ((Param Loc IntType _):ys) n state
    = case (exprCode x n state) of (s, _, IntType) -> s ++ (callCode id xs ys (n + 1) state)
                                   (_, _, _) -> error "Error: Parameter type mismatch"
callCode id (x:xs) ((Param Loc FloatType _):ys) n state
    = case (exprCode x n state) of (s, _, FloatType) -> s ++ (callCode id xs ys (n + 1) state)
                                   (s, _, IntType) -> s ++ "int_to_real r" ++ (show n) ++ ", r" ++ (show n) ++ "\n" ++ (callCode id xs ys (n + 1) state)
                                   (_, _, _) -> error "Error: Parameter type mismatch"



-- Check whether an expression is a well-formed scalar
-- A well-formed scalar is of the form id, id[e], id[e1, e2]
-- where e, e1, e2 are positive integers
isScalar :: Expr -> LocalState -> Bool
isScalar (Var (Id id)) (_, vt, _, _)
    = (Map.member id vt) && (a == 0) && (b == 0)
        where (VarInfo _ _ _ (a, b) _) = getVarInfo id vt
isScalar (Var (Array id e)) t@(_, vt, _, _)
    = (Map.member id vt) && (a > 0) && (b == 0) && (tp == IntType)
        where (VarInfo _ _ _ (a, b) _) = getVarInfo id vt
              (_, _, tp) = exprCode e 0 t
isScalar (Var (Matrix id e1 e2)) t@(_, vt, _, _)
    = (Map.member id vt) && (a > 0) && (b > 0) && (tp1 == IntType) && (tp2 == IntType)
        where (VarInfo _ _ _ (a, b) _) = getVarInfo id vt
              (_, _, tp1) = exprCode e1 0 t
              (_, _, tp2) = exprCode e1 0 t
isScalar _ _
    = False

exprCodeRef :: Expr -> Reg -> LocalState -> (String, Reg, BaseType)
exprCodeRef (BoolConst b) r _ = 
    ("    int_const" ++ regToStr(r) ++ ", "++ show(val) ++"\n",
        r,
        BoolType)
    where 
        val = boolToInt b
        exprCodeRef (IntConst i) r _ = 
            ("    int_const" ++ regToStr(r) ++ ", "++ show(i) ++"\n",
                r,
            IntType)
exprCodeRef (FloatConst f) r _ = 
    ("    real_const" ++ regToStr(r) ++ ", "++ show(f) ++"\n",
        r,
        FloatType)
exprCodeRef (StrConst s) r _ = 
    ("    string_const" ++ regToStr(r) ++ ", \""++ s ++"\"\n",
        r,
        StrType)

exprCodeRef (Var var) r state@(_, varTable, _, _) =
        case var of
            (Id varid) -> 
                let
                    varInfo = lookupVarTable varid varTable
                    isNotFP = getFpFlag varInfo
                    indic = getIndic varInfo
                    (m, n) = getDim varInfo
                in
                    if m == 0 && n == 0 then
                        if indic == Ref then 
                            ("    load" ++ regToStr(r) ++ ", " ++ show((getSlotNum varInfo)) ++ "\n",
                                r,
                                (getBaseType varInfo))
                        else
                            ("    load_address" ++ regToStr(r) ++ ", " ++ show((getSlotNum varInfo)) ++ "\n",
                                r,
                                (getBaseType varInfo)
                            )
                    else
                        error ("Error: " ++ varid ++ "is not a singleton variable\n")
            (Array varid expr) -> 
                let
                    varInfo = lookupVarTable varid varTable
                    (m, n) = getDim varInfo
                    (code1, r1, ty) = exprCode expr r state
                    code2 = "    load_address" ++ regToStr(r1+1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n"
                    code3 = "    sub_offset" ++ regToStr(r1) ++ "," ++ regToStr(r1+1) ++ "," ++ regToStr(r) ++ "\n"
                in
                    if m >0 && n == 0 then
                        (concat([code1,code2,code3]),
                            r1,
                            (getBaseType varInfo)
                        )
                    else
                        error ("Error: " ++ varid ++ "is not an array\n")

            (Matrix varid expr1 expr2) -> 
                let
                    varInfo = lookupVarTable varid varTable
                    (code1, r1, ty1) = exprCode expr1 (r+2) state
                    (code2, r2, ty2) = exprCode expr2 (r+4) state
                    (d1, d2) = getDim varInfo
                in
                    if d1 > 0 && d2 > 0 then
                        ("    load_address" ++ regToStr(r+1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n" ++
                        code1 ++ code2 ++
                        "    int_const " ++ regToStr(r+3) ++ ", " ++ show(d1) ++ "\n" ++
                        "    mul_int " ++ regToStr(r+2) ++ ", " ++ regToStr(r+2) ++ ", " ++ regToStr(r+3) ++ "\n" ++
                        "    add_int " ++ regToStr(r+2) ++ ", " ++ regToStr(r+2) ++ ", " ++ regToStr(r+4) ++ "\n" ++
                        "    sub_offset" ++ regToStr(r) ++ "," ++ regToStr(r+1) ++ "," ++ regToStr(r+2) ++ "\n",
                        r,
                        (getBaseType varInfo)
                        )
                    else
                        error ("Error: " ++ varid ++ "is not a matrix\n")


exprCodeRef (Unary unaOp expr) r state = 
    let
        (code, reg, ty) = exprCode expr r state
        code' = 
            case unaOp of
                Neg -> "    not" ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
                Minus -> "    neg_" ++ (ozTy ty) ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
    in
        (code++code', reg, ty)

exprCodeRef (Binary binOp lexpr rexpr) r state =
    let
        left@(lcode, lreg, lty) = exprCode lexpr r state
        right@(rcode, rreg, rty) = exprCode rexpr (r+1) state
        (isConvert, convertCode, commonTy) = tyConvert left right
        (code', afterType) = binOpCode binOp lreg rreg commonTy
    in
        (lcode ++ rcode ++ convertCode ++ code', lreg, afterType)

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
                    isNotFP = getFpFlag varInfo
                    indic = getIndic varInfo
                    (m, n) = getDim varInfo
                in
                    if m == 0 && n == 0 then
                        if indic /= Ref then 
                            ("    load" ++ regToStr(r) ++ ", " ++ show((getSlotNum varInfo)) ++ "\n",
                                r,
                                (getBaseType varInfo))
                        else
                            ("    load" ++ regToStr(r) ++ ", " ++ show((getSlotNum varInfo)) ++ "\n" ++
                            "    load_indirect" ++ regToStr(r) ++ ", " ++ regToStr(r) ++ "\n" ,
                                r,
                                (getBaseType varInfo)
                            )
                    else
                        error ("Error: " ++ varid ++ "is not a singleton variable\n")

            (Array varid expr) -> 
                let
                    varInfo = lookupVarTable varid varTable
                    (m, n) = getDim varInfo
                    (code1, r1, ty) = exprCode expr r state
                    code2 = "    load_address" ++ regToStr(r1+1) ++ "," ++ show(getSlotNum (varInfo)) ++ "\n"
                    code3 = "    sub_offset" ++ regToStr(r1) ++ "," ++ regToStr(r1+1) ++ "," ++ regToStr(r) ++ "\n"
                    code4 = "    load_indirect" ++ regToStr(r1) ++ "," ++ regToStr(r1) ++ "\n"
                in
                    if m > 0 && n == 0 then
                        (concat([code1,code2,code3,code4]),
                            r,
                            (getBaseType varInfo)
                        )
                    else
                        error ("Error: " ++ varid ++ "is not an array\n")

            (Matrix varid expr1 expr2) -> 
                let
                    varInfo = lookupVarTable varid varTable
                    (code1, r1, ty1) = exprCode expr1 (r+2) state
                    (code2, r2, ty2) = exprCode expr2 (r+4) state
                    (d1, d2) = getDim varInfo
                in
                    if d1 > 0 && d2 > 0 then
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
                    else
                        error ("Error: " ++ varid ++ "is not a matrix\n")

exprCode (Unary unaOp expr) r state = 
    let
        (code, reg, ty) = exprCode expr r state
        code' = if ty == BoolType then
                    if  unaOp == Neg then
                        "    not" ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
                    else
                        error "Error: Cannot perform negation on non-boolean type"
                else if unaOp == Minus then
                    "    neg_" ++ (ozTy ty) ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
                    else
                        error "Error: Cannot perform unary minus on non-numeric type"
    in
        (code++code', reg, ty)

exprCode (Binary binOp lexpr rexpr) r state =
    let
        left@(lcode, lreg, lty) = exprCode lexpr r state
        right@(rcode, rreg, rty) = exprCode rexpr (r+1) state
        (isConvert, convertCode, commonTy) = tyConvert left right
        (code', afterType) = binOpCode binOp lreg rreg commonTy
    in
       if (classifyBinOp binOp) == Logic then
            if (isBothBool lty rty) then
                (lcode ++ rcode ++ convertCode ++ code', lreg, afterType)
            else
                error ("ERROR: Non-Boolean Logical Operators")
        else if (classifyBinOp binOp) == Arithmetic then
            if (isBothNumeric lty rty) then
                (lcode ++ rcode ++ convertCode ++ code', lreg, afterType)
            else
                error ("ERROR: Non-Numeric Arithmetical Operators")
        else if binOp == Eq || binOp == NotEq then
            if lty == rty then
                (lcode ++ rcode ++ convertCode ++ code', lreg, afterType)
            else
                error ("ERROR: Incompatible Types for Equal or NotEqual")
        else
            if (isBothNumeric lty rty) || (isBothBool lty rty) then
                (lcode ++ rcode ++ convertCode ++ code', lreg, afterType)
            else
                error ("ERROR: Incompatible Types for Comparison")

isBothNumeric :: BaseType -> BaseType -> Bool
isBothNumeric IntType FloatType = True
isBothNumeric IntType IntType = True
isBothNumeric FloatType FloatType = True
isBothNumeric FloatType IntType = True
isBothNumeric _ _ = False

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
    if (isBothBool ty1 ty2) then
        (False, "", BoolType)
    else
        if (isBothSameType ty1 ty2) then
            (False, "", ty1)
        else
            let 
                reg = convertReg (reg1,ty1) (reg2,ty2) 
            in
                (True , "    int_to_real" ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n", FloatType)

isBothBool :: BaseType -> BaseType -> Bool
isBothBool BoolType BoolType = True
isBothBool _ _ = False

isBothSameType :: BaseType -> BaseType -> Bool
isBothSameType ty1 ty2 = ty1 == ty2 

convertReg :: (Reg, BaseType) -> (Reg, BaseType) -> Reg
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


