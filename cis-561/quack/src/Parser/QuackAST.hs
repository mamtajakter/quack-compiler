{-# LANGUAGE GADTs #-}

module Parser.QuackAST where

data Prog where
  Prog :: [Class] -> [Statement] -> Prog
  deriving (Show)

data Class where
  Class :: ClassSig -> ClassBody -> Class
  deriving (Show)


data ClassSig where
  ClassSig :: String -> FormArgs -> Maybe String -> ClassSig
  deriving (Show)

type FormArgs = [TyAssign]

data TyAssign = TyAssign (String, String) deriving (Show, Eq) -- variable : type

data Statement where
  Assignment :: LExpr -> Maybe String -> RExpr -> Statement
  StatementR :: RExpr -> Statement
  If         :: (RExpr, StmtBlock)
             -> [Statement]       -- can only be an if statement though!
             -> Maybe StmtBlock         -> Statement
  While      :: (RExpr, StmtBlock)      -> Statement
  Return     :: Maybe RExpr             -> Statement
  TyCase     :: RExpr                   -> [TyAlt] -> Statement
  deriving Show

type TyAlt = (TyAssign, StmtBlock)

data RExpr where
  Plus   :: RExpr   -> RExpr   -> RExpr   -- not present in AST
  Minus  :: RExpr   -> RExpr   -> RExpr   -- not present in AST
  Times  :: RExpr   -> RExpr   -> RExpr   -- not present in AST
  Div    :: RExpr   -> RExpr   -> RExpr   -- not present in AST
  Neg    :: RExpr   -> RExpr
  Paren  :: RExpr   -> RExpr              -- not present in AST
  Str    :: String  -> RExpr
  Int    :: Integer -> RExpr
  Bool   :: Bool -> RExpr 
  LExpr  :: LExpr   -> RExpr
  PEQ     :: RExpr   -> RExpr   -> RExpr  -- not present in AST
  PLEQ    :: RExpr   -> RExpr   -> RExpr  -- not present in AST
  GEQ    :: RExpr   -> RExpr   -> RExpr   -- not present in AST
  PLT     :: RExpr   -> RExpr   -> RExpr  -- not present in AST
  PGT     :: RExpr   -> RExpr   -> RExpr  -- not present in AST
  AND    :: RExpr   -> RExpr   -> RExpr
  OR     :: RExpr   -> RExpr   -> RExpr
  NOT    :: RExpr   -> RExpr
  RArgs  :: RExpr   -> String  -> ActArgs -> RExpr
  Constr :: String  -> ActArgs -> RExpr
  deriving Show


type ActArgs = Maybe [RExpr]

data LExpr where
  Lid     :: String -> LExpr
  ObjField :: RExpr  -> String -> LExpr
  deriving Show

data ClassBody where
  ClassBody :: [Statement] -> [Method] -> ClassBody
  deriving (Show)

data Method where
  -- class, method name, arguments and their types, return type, stmtblock
   Method :: String -> String -> FormArgs -> Maybe String -> StmtBlock -> Method
   deriving Show

type StmtBlock = [Statement]







desugarRexprHelper :: String -> RExpr -> RExpr -> RExpr
desugarRexprHelper str rexp1 rexp2 = case (desugarRexpr rexp1, desugarRexpr rexp2) of
      (LExpr (Lid s1), LExpr (Lid s2))    -> RArgs (LExpr (Lid s1)) str (Just [LExpr (Lid s2)])
      (LExpr (Lid s1), r2)                -> RArgs (LExpr (Lid s1)) str (Just [desugarRexpr r2])
      (r1, LExpr (Lid s2))                -> RArgs (desugarRexpr r1) str (Just [LExpr (Lid s2)])
      (r1, r2)                            -> RArgs (desugarRexpr r1) str (Just [desugarRexpr r2])

desugarRexpr :: RExpr -> RExpr
desugarRexpr      (Plus e1 e2)                = desugarRexprHelper "PLUS" e1 e2
desugarRexpr      (Minus e1 e2)               = desugarRexprHelper "Minus" e1 e2
desugarRexpr      (Times e1 e2)               = desugarRexprHelper "Times" e1 e2
desugarRexpr      (Div e1 e2)                 = desugarRexprHelper "Divide" e1 e2
desugarRexpr      (Paren e)                   = desugarRexpr e
desugarRexpr      str@(Str s)                 = str
desugarRexpr      int@(Int i)                 = int
desugarRexpr      lexp@(LExpr l)              = lexp
desugarRexpr      neg@(Neg l)                 = neg
desugarRexpr      (PEQ e1 e2)                 = desugarRexprHelper "EQUALS" e1 e2
desugarRexpr      (PLEQ e1 e2)                = OR (desugarRexprHelper "LESS" e1 e2) (desugarRexprHelper "EQUALS" e1 e2)
desugarRexpr      (GEQ e1 e2)                 = NOT (desugarRexprHelper "LESS" e1 e2)
desugarRexpr      (PLT e1 e2)                 = desugarRexprHelper "LESS" e1 e2
desugarRexpr      (PGT e1 e2)                 = NOT (desugarRexpr (PLEQ e1 e2))
desugarRexpr      por@(OR _ _)        = por
desugarRexpr      pand@(AND _ _)      = pand
desugarRexpr      pnot@(NOT _)        = pnot
desugarRexpr      rargs@RArgs {} = rargs
desugarRexpr      constr@(Constr _ _) = constr
