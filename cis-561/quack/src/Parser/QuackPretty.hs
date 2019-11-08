{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Parser.QuackPretty where

-- import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine

import Parser.QuackAST
import Data.Data

prettyRExpr :: RExpr -> String
prettyRExpr rexp = case rexp of
     Plus exp1 exp2   -> prettyRExpr exp1 ++ " + " ++ prettyRExpr exp2
     Minus exp1 exp2  -> prettyRExpr exp1 ++ " - " ++ prettyRExpr exp2
     Times exp1 exp2  -> prettyRExpr exp1 ++ " * " ++ prettyRExpr exp2
     Div   exp1 exp2  -> prettyRExpr exp1 ++ " / " ++ prettyRExpr exp2
     Neg   exp1       -> "-" ++ prettyRExpr exp1
     Paren exp1       -> "(" ++ prettyRExpr exp1 ++ ")"
     Str   str        -> str
     Int   i          -> show i
     LExpr lexpr      -> prettyLExpr lexpr
     PEQ   exp1 exp2  -> prettyRExpr exp1 ++ " = " ++ prettyRExpr exp2
     PLEQ  exp1 exp2  -> prettyRExpr exp1 ++ " <= " ++ prettyRExpr exp2
     GEQ   exp1 exp2  -> prettyRExpr exp1 ++ " >= " ++ prettyRExpr exp2
     PLT   exp1 exp2  -> prettyRExpr exp1 ++ " < " ++ prettyRExpr exp2
     PGT   exp1 exp2  -> prettyRExpr exp1 ++ " > " ++ prettyRExpr exp2
     AND   exp1 exp2  -> prettyRExpr exp1 ++ " and " ++ prettyRExpr exp2
     OR    exp1 exp2  -> prettyRExpr exp1 ++ " or "  ++ prettyRExpr exp2
     NOT   exp1       -> "not " ++ prettyRExpr exp1
     RArgs exp1 str args -> prettyRExpr exp1 ++ "." ++ str ++ "(" ++ prettyActArgs args ++ ")"
     Constr exp1 args   -> exp1 ++ "(" ++ prettyActArgs args ++ ")"

prettyLExpr :: LExpr -> String
prettyLExpr lexpr = case lexpr of
    Lid str           -> str
    LObjLoc rexpr str -> prettyRExpr rexpr ++ "." ++ str

prettyActArgs :: ActArgs -> String
prettyActArgs actargs = case actargs of
    Nothing            -> ""
    Just rexps         -> unwords $ map (\r -> prettyRExpr r ++ ", ") rexps

prettyStmtBlock :: StmtBlock -> String
prettyStmtBlock stmb = "{\n" ++ (unwords $ map prettyStatement stmb) ++ "\n}"

prettyRexpStBlock ::  [Statement] -> String
prettyRexpStBlock = unwords.map (\stmt -> case stmt of
              If (rexp, stB) [] Nothing -> "else if" ++ "(" ++ prettyRExpr rexp ++ ")" ++ prettyStmtBlock stB;
              _                         -> "ERROR")

prettyTyAssign :: TyAssign -> String
prettyTyAssign (TyAssign (str1, str2)) = str1 ++ " : " ++ str2

prettyTyAlt    :: TyAlt  -> String
prettyTyAlt (tyassign, stmtblk) = prettyTyAssign tyassign ++ prettyStmtBlock stmtblk


prettyStatement :: Statement -> String
prettyStatement stmt = case stmt of
     Statement lexpr mbstr rexpr -> prettyLExpr lexpr ++ (maybe "" id mbstr) ++ " = " ++ (prettyRExpr rexpr) ++ ";\n"
     StatementR rexpr            -> prettyRExpr rexpr ++ ";\n"
     If (rexpr, stmtblk) rexpstmblist mbstblock -> let stblock =
                                                        maybe "" (\b -> "else " ++ prettyStmtBlock b) mbstblock
                                                         in unlines [
                                                           "if ", prettyRExpr rexpr, prettyStmtBlock stmtblk
                                                           , prettyRexpStBlock  rexpstmblist
                                                           , stblock, "\n"
                                                           ]
     While (rexpr, stmtblk)       -> "while " ++ prettyRExpr rexpr ++ prettyStmtBlock stmtblk ++ "\n"
     Return mbrexpr               -> "return " ++ maybe "" prettyRExpr mbrexpr ++ ";\n"
     TyCase rexpr tyaltlist       -> "typecase " ++ prettyRExpr rexpr  ++ "{\n" ++ (unlines $ (map prettyTyAlt tyaltlist)) ++ "\n}"


maybelist :: String -> ([a] -> String) -> [a] -> String
maybelist b f l = if null l then b else f l

prettyMethod :: Method -> String
prettyMethod (Method cls str fargs mbstr stmblk) = "def " ++ str ++ "(" ++ prettyFormArgs fargs ++ ")" ++ ": " ++ (maybe "" id mbstr) ++ "\n" ++ (prettyStmtBlock stmblk) ++ "\n}"

prettyClassBody :: ClassBody -> String
prettyClassBody (ClassBody stmtl mtdl) = "{\n" ++ maybelist "" prettyStmtBlock stmtl ++ (unlines $ map (\m -> prettyMethod m ++ "\n\n") mtdl) ++ "\n"

prettyClassSig :: ClassSig -> String
prettyClassSig (ClassSig str fargs mbstr) = "class " ++ str ++ " (" ++ (prettyFormArgs fargs) ++ ") " ++ (maybe "" (\s -> "extends " ++ s) mbstr)

prettyFormArgs :: FormArgs -> String
prettyFormArgs  = unwords.map (\tyassign -> "," ++ prettyTyAssign tyassign)

prettyClass :: Class -> String
prettyClass (Class clsig clbody) = (prettyClassSig clsig) ++ prettyClassBody clbody

prettyProg :: Prog -> String
prettyProg (Prog clist stlist) = (unwords $ map prettyClass clist) ++ prettyStmtBlock stlist





----------------------------------------------------------------------
-------------------- Draw AST using Diagrams? ------------------------
----------------------------------------------------------------------

-- node :: Int -> Diagram B
-- node n = text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green

-- drawAST :: Prog -> Diagram B     -- can this program be generalized?? -- generic programming????
-- drawAST = undefined
--
--
-- drawRExpr :: RExpr -> Diagram B
-- drawRExpr rexp = undefined
