module SymbolTable where

import qualified Data.Map as Map
import GoatAST
-- import Analyze

type SymTable 
    = (CallTable, ProcTable)

type CallTable
    = Map.Map Ident [Param]

type ProcTable
    = Map.Map Ident VarTable

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

-- -- calculate the size of the stack frame needed for a procedure
-- getSize :: VarTable -> Int
-- getSize t
--   = Map.fold sumSize 0 t

-- sumSize :: VarInfo -> Int -> Int
-- sumSize (VarInfo _ _ False _ _) n
--   = n + 1
-- sumSize (VarInfo _ _ True (0, 0) _) n
--   = n + 1
-- sumSize (VarInfo _ _ True (a, 0) _) n
--   = n + a
-- sumSize (VarInfo _ _ True (a, b) _) n
--   = n + a * b

genSymTable :: Program -> SymTable
genSymTable program =
    let
        callTable = genCallTable program
        -- procTable = genProcTable program
        procTable = Map.empty
    in
        (callTable, procTable)

genCallTable :: Program -> CallTable
genCallTable (Program procs) =
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

getIdentFromProc :: Proc -> Ident
getIdentFromProc (Proc id _ _ _) = id

getParamsFromProc :: Proc -> [Param]
getParamsFromProc (Proc _ params _ _) = params

