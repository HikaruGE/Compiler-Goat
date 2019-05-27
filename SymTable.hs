module SymTable where

import qualified Data.Map as Map
import GoatAST
-- import Analyze

-- type ProcTable = String

data ProcTable
  = Map Ident ProcInfo

-- formal parameter table, local variable table
data ProcInfo
  = ProcInfo FPTable LVTable

data FPTable
  = Map Ident FPInfo

-- type, parameter passing method, slot number
data FPInfo
  = FPInfo BaseType Indic Int

data LVTable
  = Map Ident LVInfo

-- type, bounds for arrays and matrices(1 for singleton variable), starting slot number
data LVInfo
  = LVInfo BaseType Int Int

initProc :: ProcTable
initProc
  = Map.empty

initFP :: FPTable
initFP
  = Map.empty

initLV :: LVTable
initLV
  = Map.empty

--addProc :: Ident -> ProcInfo -> ProcTable -> ProcTable
--addProc k v m
--  = Map.insert k v m

addProc :: Proc -> ProcTable -> ProcTable
addProc x@(Proc id _ _ _) m
  = Map.insert id (genProcInfo x) m

--addFP :: Ident -> FPInfo -> FPTable -> FPTable
--addFP k v m
--  = Map.insert k v m

addFP :: Param -> FPTable -> FPTable
addFP x@(Param _ _ id) m
  = Map.insert id (genFPInfo x) m

--addLV :: Ident -> LVInfo -> LVTable -> LVTable
--addLV k v m
--  = Map.insert k v m

addLV :: Decl -> LVTable -> LVTable
addLV x@(DeclVar _ id) m
  = Map.insert id (genLVInfo x) m
addLV x@(DeclArray _ id _) m
  = Map.insert id (genLVInfo x) m
addLV x@(DeclMatrix _ id _ _) m
  = Map.insert id (genLVInfo x) m

-- look up names that is guaranteed to be present
getProcInfo :: Ident -> ProcTable -> ProcInfo
getProcInfo id table
  = m
    where (Just m) = Map.lookup id table

getFPTable :: Ident -> ProcTable -> FPTable
getFPTable id table
  = m
    where (ProcInfo m _) = getProcInfo id table

getFPInfo :: Ident -> FPTable -> FPInfo
getFPInfo id table
  = m
    where (Just m) = Map.lookup id table

getLVTable :: Ident -> ProcTable -> LVTable
getLVTable id table
  = m
    where (ProcInfo _ m) = getProcInfo id table

getLVInfo :: Ident -> LVTable -> LVInfo
getLVInfo id table
  = m
    where (Just m) = Map.lookup id table

-- calculate the size of the stack frame needed for a procedure
getSize :: Ident -> ProcTable -> Int
getSize id table
  = (getFPSize id table) + (getLVSize id table)

getFPSize :: Ident -> ProcTable -> Int
getFPSize id table
  = Map.size (getFPTable id table)

getLVSize :: Ident -> ProcTable -> Int
getLVSize id table
  = Map.fold sumSize 0 (getLVTable id table)

sumSize :: LVInfo -> Int -> Int
sumSize (LVInfo _ m _) n
  = m + n
