-- Team Lihai

module SymbolTable where

import qualified Data.Map as Map
import GoatAST
-- import Analyze

type SymTable 
    = (CallTable, ProcTable)

-- A table that maps a procedure to its parameter list
type CallTable
    = Map.Map Ident [Param]

getCallList :: Ident -> CallTable -> [Param]
getCallList id table
    = case (Map.lookup id table) of Just m -> m
                                    Nothing -> error ("Error: Procedure \"" ++ id ++ "\" is not defined")

-- A table that maps a procedure to its local table of parameter and variable information
type ProcTable
    = Map.Map Ident VarTable

-- A table that maps a formal parameter or local variable to its own information
type VarTable
    = Map.Map Ident VarInfo

-- True represents local variable, False represents formal parameter
-- Int pair represents the index bounds, 0 if this dimension not needed
-- (0, 0) represents singleton variable
-- (a, 0) represents array, where a >= 1
-- (a, b) represents matrix, where a, b >= 1
type SlotNum = Int
data VarInfo
    = VarInfo BaseType SlotNum Bool (Int, Int) Indic
        deriving(Show)
getBaseType :: VarInfo -> BaseType
getBaseType (VarInfo ty _ _ _ _) = ty
getSlotNum :: VarInfo -> SlotNum
getSlotNum (VarInfo _ slot _ _ _) = slot
getDim :: VarInfo -> (Int, Int)
getDim (VarInfo _ _ _ (a,b) _) = (a,b)
getIndic :: VarInfo -> Indic
getIndic (VarInfo _ _ _ _ indic) = indic
getFpFlag :: VarInfo -> Bool
getFpFlag (VarInfo _ _ f _ _) = f

-- look up a procedure name that is guaranteed to be present, from the global symbol table
getVarTable :: Ident -> ProcTable -> VarTable
getVarTable id table
    = m
        where (Just m) = Map.lookup id table


-- look up a variable name that is guaranteed to be present, from the local symbol table
getVarInfo :: Ident -> VarTable -> VarInfo
getVarInfo id table
    = m
    where (Just m) = Map.lookup id table

-- calculate the size of the stack frame needed for a procedure
getSize :: VarTable -> Int
getSize t
  = Map.foldr sumSize 0 t

sumSize :: VarInfo -> Int -> Int
sumSize (VarInfo _ _ False _ _) n
  = n + 1
sumSize (VarInfo _ _ True (0, 0) _) n
  = n + 1
sumSize (VarInfo _ _ True (a, 0) _) n
  = n + a
sumSize (VarInfo _ _ True (a, b) _) n
  = n + a * b

initSymTable :: Program -> SymTable
initSymTable program =
    let
        callTable = initCallTable program
        procTable = initProcTable program
        -- procTable = Map.empty
    in
        (callTable, procTable)

initCallTable :: Program -> CallTable
initCallTable (Program procs) =
    let
        init = Map.empty 
    in
        foldl insertCallTable init procs

insertCallTable :: CallTable -> Proc -> CallTable
insertCallTable table proc = 
    let 
        procid = getIdentFromProc proc
        params = getParamsFromProc proc
    in
        Map.insert procid params table

initProcTable :: Program -> ProcTable
initProcTable (Program procs) =
    let
        init = Map.empty 
    in
        foldl insertProcTable init procs

insertProcTable :: ProcTable -> Proc -> ProcTable
insertProcTable table proc =
    let
        procid = getIdentFromProc proc
        varTable = initVarTable proc
    in
        if Map.member procid table then
            error ("Error: Duplicated definiton of procedure \"" ++ procid ++ "\"\n")
        else        
            Map.insert procid varTable table

initVarTable :: Proc -> VarTable
initVarTable proc =
    let
        init = Map.empty
        params = getParamsFromProc proc
        decls = getDeclsFromProc proc
        varIdVarInfosPairs = genVarInfos params decls 0
    in
        foldl insertVarTable init varIdVarInfosPairs

insertVarTable :: VarTable -> (Ident, VarInfo) -> VarTable
insertVarTable table (id, varInfo) =
    if Map.member id table then
        error ("Error: Duplicated definition of variable \"" ++ id ++ "\"\n")
    else
        Map.insert id varInfo table

lookupVarTable :: Ident -> VarTable -> VarInfo
lookupVarTable varid vt = 
    let
        result = Map.lookup varid vt
    in
        case result of
            Just v -> v
            Nothing -> error ("Error: Variable \""++ varid ++ "\" is not defined.")

genVarInfos :: [Param] -> [Decl] -> Int -> [(Ident, VarInfo)]
genVarInfos [] [] _ = []
genVarInfos (p@(Param _ _ id):ps) decls n = 
    (id, varInfo):genVarInfos ps decls (n+1)
    where
        varInfo = genVarInfoFromParam p n
genVarInfos [] (d:ds) n =
    case d of
        DeclVar ty id           ->  (id, varInfo):genVarInfos [] ds (n+1)
        DeclArray ty id a       ->  (id, varInfo):genVarInfos [] ds (n+a)
        DeclMatrix ty id a b    ->  (id, varInfo):genVarInfos [] ds (n+a*b)
    where
        varInfo = getVarInfoFromDecl d n

genVarInfoFromParam :: Param -> SlotNum -> VarInfo
genVarInfoFromParam (Param indic baseType ident) slotNum =
    VarInfo baseType slotNum False (0,0) indic

getVarInfoFromDecl :: Decl -> SlotNum -> VarInfo
getVarInfoFromDecl (DeclVar baseType ident) slotNum
    = VarInfo baseType slotNum True (0,0) Loc
getVarInfoFromDecl (DeclArray baseType ident a) slotNum
    = if a > 0 then
        VarInfo baseType slotNum True (a,0) Loc
      else
        error "Error: Invalid array size"
getVarInfoFromDecl (DeclMatrix baseType ident a b) slotNum
    = if (a > 0) && (b > 0) then
        VarInfo baseType slotNum True (a,b) Loc
      else
        error "Error: Invalid matrix size"

getIdentFromProc :: Proc -> Ident
getIdentFromProc (Proc id _ _ _) = id

getParamsFromProc :: Proc -> [Param]
getParamsFromProc (Proc _ params _ _) = params

getDeclsFromProc :: Proc -> [Decl]
getDeclsFromProc (Proc _ _ decls _) = decls

