module Analyze where

import GoatAST
import SymTable

genSymTable :: Program -> ProcTable
genSymTable (Program m)
  = genProc Map.empty m

genProc :: ProcTable -> [Proc] -> ProcTable
genProc m []
  = m
genProc m x:xs
--  = genProc (addProc x m) xs
  = case x of (Proc id _ _ _) -> genProc (Map.insert id (genProcInfo x) m) xs

genProcInfo :: Proc -> ProcInfo
genProcInfo (Proc _ x y _)
  = ProcInfo (genFP Map.empty x 0) (genLV Map.empty y (length x))

genFP :: FPTable -> [Param] -> Int -> FPTable
genFP m [] _
  = m
genFP m x:xs n
--  = genFP (addFP x m) xs
  = case x of (Param _ _ id) -> genFP (Map.insert id (genFPInfo x n) m) xs (n + 1)

genLV :: LVTable -> [Decl] -> Int -> LVTable
genLV m [] _
  = m
genLV m x:xs n
--  = genLV (addLV x m) xs
  = case x of (DeclVar _ id) -> genLV (Map.insert id (genLVInfo x n) m) xs (n + 1)
              (DeclArray _ id a) -> genLV (Map.insert id (genLVInfo x n) m) xs (n + a)
              (DeclMatrix _ id a b) -> genLV (Map.insert id (genLVInfo x n) m) xs (n + a * b)

genFPInfo :: Param -> Int -> FPInfo
genFPInfo (Param x y _) n
  = FPInfo y x n

genLVInfo :: Decl -> Int -> LVInfo
genLVInfo (DeclVar x _) n
  = LVInfo x 1 n
genLVInfo (DeclArray x _ a) n
  = LVInfo x a n
genLVInfo (DeclMatrix x _ a b) n
  = LVInfo x (a * b) n
