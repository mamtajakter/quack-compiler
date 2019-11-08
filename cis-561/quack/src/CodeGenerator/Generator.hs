{-# LANGUAGE MultiParamTypeClasses #-}
module CodeGenerator.Generator where

import TypeChecker.QuackTypes
import TypeChecker.TypeCheck
import qualified TypeChecker.QuackTypes as T
import qualified Parser.QuackAST as P
import Parser.QuackAST
import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Control.Monad.Plus
import Control.Applicative
import Control.Monad.Identity
import Data.List 


type CCode = String

empty_cdGen = CodeGenData {
                  ctable=M.empty
                 , class_name=Nothing
                 , cvartable=M.empty
                 , tyctx=M.empty
                 , varstack=[]
                 , methd_rety = Nothing
                 , count_if = 0
                 , count_while = 0
                 , count_var = 0
                 , in_constr = False
                 , code = " // Starting code generation session: "
                 , super = "Obj"
                 , curr_formargs = []
                 , localvars = []
                 , var_now_name = ""
                 -- interesting question: can I have it so it always tracks changes made in ctable??
                 , method_sig_t = M.empty  
                 , assigned = []

              }

data CodeGenData = CodeGenData {
                      ctable        :: CTable
                    , class_name    :: Maybe Name
                    , cvartable     :: CVarTable
                    , tyctx         :: TyCtx
                    , varstack      :: [(String, String)] -- tempname, type 
                    , methd_rety    :: Maybe String
                    , count_if      :: Int
                    , count_while   :: Int
                    , count_var     :: Int
                    , in_constr     :: Bool
                    , code          :: CCode
                    , super         :: String
                    , curr_formargs :: FormArgs
                    , localvars     :: [(String, String)] -- tempname, type 
                    , var_now_name  :: String
                    , method_sig_t  :: MethodT 
                    , assigned      :: [(String, String)]
                  } deriving (Show)


-- If labels
data IfLabel = IfLabel {
                         true_label  :: String
                       , false_label :: String
                       , end_label   :: String
                      }

data WhileLabel = WhileLabel {
                               again_label :: String
                             , test_label  :: String
                             }


data MCodeGen a = MGenerator {runGen :: CodeGenData -> (a, CodeGenData)}

instance Functor MCodeGen where
  fmap f mcg = MGenerator $ \cgd ->
                     case runGen mcg cgd of
                       (a, code) -> (f a, code)

instance Applicative MCodeGen where
  pure = return
  (<*>)  = ap

instance Monad MCodeGen where
  return x   = MGenerator $ \cgd -> (x, cgd)
  m >>=  cd  = MGenerator $ \cgd -> case runGen m cgd of
                                    (a, code) -> runGen (cd a) code


instance MonadState CodeGenData MCodeGen where
  get           = MGenerator $ \cgd -> (cgd, cgd)
  put cdata     = MGenerator $ const ((), cdata)
  state func    = MGenerator $ \cgd -> func cgd



genIfLabel :: Int -> IfLabel
genIfLabel  c = IfLabel {true_label= "TLABEL_" ++ show c 
                        , false_label= "FLABEL_" ++ show c 
                        , end_label= "ELABEL_" ++ show c }

genWhileLabel :: Int -> WhileLabel
genWhileLabel  c = WhileLabel {
                            again_label="AG_LABEL_" ++ show c 
                          , test_label ="TEST_LABEL_"  ++ show c 
                              }

genProg :: MCodeGen CodeGenData -> Prog -> IO (MCodeGen CodeGenData)
genProg mgen (Prog cl_list stmt_list) = return $ do
                                code_gen_data <- mgen
                                new_data      <- genClassList cl_list
                                stm_data      <- genStmtBlock code_gen_data stmt_list
                                let new_code =   unlines [
                                             "#include \"Builtins.h\""
                                            -- , "#include \"Builtins.h\""
                                            , "#include <stdio.h>"
                                            , "#include <stdlib.h>"
                                            , "#include <string.h>"
                                            , "void quackmain();"
                                            , "int main(int argc, char** argv){"
                                            , "quackmain();"
                                            , "printf(\"------Terminated successfully (woot!)------\");\n"
                                            , "exit(0);\n}\n"
                                           , (code new_data)
                                           , "void quackmain(){\n"
                                           , (code stm_data)
                                           , "}"]
                                -- put code_gen_data{code=new_code})
                                -- to_return <- get
                                return code_gen_data{code=new_code}

genClassList :: [Class] -> MCodeGen CodeGenData
genClassList  l = do 
  code_gen_data <- get 
  foldM (\old_data cl -> do
                        new_data <- genClass cl
                        return code_gen_data{code=unlines [code old_data, code new_data]}) code_gen_data l 


genClass :: Class -> MCodeGen CodeGenData
genClass (Class cl_sig cl_body) = do
                  -- let c_data = code_gen_data {class_name = Just name}
                  -- code_gen_data <- get 
                  sig_data   <- genClassSig cl_sig
                  body_data  <- genClassBody cl_body  -- might wana change to use results from sig.
                  let class_code = unlines [code sig_data , code body_data]
                  put body_data{code=class_code}
                  return body_data{code=class_code}

genClassSig :: ClassSig -> MCodeGen CodeGenData
genClassSig (ClassSig name f_args exten) = do
                                    code_gen_data <- get 
                                    put code_gen_data{class_name=Just name}
                                    -- form_arg_data <- genFormArgs code_gen_data True f_args
                                    form_arg_data <- genFields
                                    let new_code = (unlines [
                                           "struct class_" ++ name ++ "_struct;"
                                          , "typedef struct class_" ++ name ++ "_struct*  class_" ++ name ++ ";"
                                          , "typedef struct obj_" ++ name ++ "_struct { "
                                          , "class_" ++ name ++ " clazz;"
                                          , code form_arg_data
                                          , "}* obj_" ++ name ++ ";"
                                          ])
                                    let new_data = form_arg_data{class_name=Just name, super=fromJust exten, curr_formargs=f_args, code=new_code}
                                    put new_data 
                                    return new_data

genClassMethodSig :: MCodeGen CodeGenData
genClassMethodSig = do 
             code_gen_data <- get 
             let cl_name  = class_name code_gen_data
             let cl_table = ctable code_gen_data
             let class_meth_table = case M.lookup (fromJust cl_name) cl_table of 
                                              Nothing -> error ("class" ++ show cl_name ++ "not in class table")
                                              Just t  -> t 
             let table_code = genMSigTable class_meth_table 
             put code_gen_data{code=table_code}
             return code_gen_data{code=table_code}

genMSigTable :: MSigTable -> CCode
genMSigTable = concatMap genMSig.(M.toList)

genMSig :: (Name, (QuackType, [QuackType])) -> CCode
genMSig (mname, (super, sig)) = "obj_" ++ getTypeName (last sig) ++ " (* " ++ mname ++ ") " 
                                ++ "(" ++ (if null $ init sig then "" else init (concatMap (++ ",") (map (\t -> "obj_" 
                                ++ getTypeName t) $ init sig))) ++ ");\n" 
                                


-- change to use cvartable   -- need localvars table -- time to refactor typechecker!
genConstruct :: CodeGenData -> Name -> FormArgs -> StmtBlock -> MCodeGen CodeGenData
genConstruct code_gen_data name f_args stmtb =  do
                                  --  f_argsStr_data <- genFormArgs code_gen_data False f_args
                                   f_argsStr_data <- get 
                                  --  genInlineFargs :: Name -> Bool -> FormArgs -> CCode
                                   let fargsStr = genInlineFargs name True f_args
                                   stm_block_data <- genStmtBlock  f_argsStr_data{code=""} stmtb --empty_cdGen
                                   let new_code = unlines [
                                          (if null f_args then  "obj_" ++ name ++ " new_" ++ name ++ fargsStr ++ "{\n"
                                            else "obj_" ++ name ++ " new_" ++ name ++  fargsStr ++ " {\n")
                                         , "/*"
                                         , (code f_argsStr_data)
                                         , fargsStr
                                         , "*/"
                                         , "obj_" ++ name ++ " this " ++ "= (" ++ "obj_" ++ name ++ ")"
                                         , "\tmalloc(sizeof(struct obj_" ++ name ++ "_struct));"
                                         , "this -> clazz = the_class_" ++ name ++ ";"
                                         , code stm_block_data
                                         , "return this;"
                                         , "\n}"
                                         ]
                                   return code_gen_data{code=new_code}

genFields :: MCodeGen CodeGenData
genFields = do 
       code_gen_data <- get 
       let cl_name   = fromJust $ class_name code_gen_data
       let cl_fields = fromJust $ M.lookup cl_name (cvartable code_gen_data)
       let fld_code = unlines [genFlds $ fst cl_fields, genFlds $ snd cl_fields] 
       let new_vars = (updateVars $ fst cl_fields) ++ (updateVars $ snd cl_fields)
       put code_gen_data{code=fld_code, varstack=new_vars ++ (varstack code_gen_data)}
       return code_gen_data{code=fld_code, varstack=new_vars ++ (varstack code_gen_data)}

genFlds :: TyCtx -> CCode
genFlds = concatMap genFld.(M.toList)

updateVars :: TyCtx -> [(Name, String)]
updateVars = map (\(n, t) -> (n, getTypeName t)).(M.toList)

genFld :: (Name, QuackType) -> CCode 
genFld (varname, ty) = "obj_" ++ (getTypeName ty) ++ " " ++ varname ++ ";\n"




genClassBody :: ClassBody -> MCodeGen CodeGenData    -- (Name, Name) refers to this, and superclass.
genClassBody (ClassBody stmt_list methd_list) = do
                              code_gen_data  <- genClassMethodSig
                              let name           = case class_name code_gen_data of
                                                          Nothing -> ""
                                                          Just nm -> nm
                              let msuper          = super code_gen_data  -- might run you into trouble
                              let f_args         = curr_formargs code_gen_data
                              let inl_fargs_data = genInlineFargs name True f_args
                              constr_data        <- genConstruct (code_gen_data{in_constr=not (in_constr code_gen_data)}) name f_args stmt_list
                              methd_list_data    <- genMethodList code_gen_data methd_list
                              methd_obj_data     <- genMethodObjList code_gen_data methd_list
                              let new_code = unlines [
                                    "struct class_" ++ name ++ "_struct the_class_" ++ name ++ "_struct;"
                                    , "struct class_" ++ name ++ "_struct {"
                                    , "class_" ++ msuper ++ "  clazz; "
                                    , code code_gen_data
                                    , "//" ++ show (M.lookup name $ cvartable code_gen_data)
                                    , "};"
                                    , "extern class_" ++ name ++ " the_class_" ++ name ++ ";"
                                    , (code constr_data)
                                    , (code methd_list_data)
                                    , (code methd_obj_data)
                                    , "class_" ++ name ++ " the_class_" ++ name ++ " =  &the_class_" ++ name ++ "_struct;"
                                    ]
                              put methd_list_data{code=new_code}
                              return methd_list_data{code=new_code}



genInlineFargs' ::  Bool -> FormArgs -> CCode
genInlineFargs' _ [] = ""
genInlineFargs' b (TyAssign (vname, vtype):ts) = if b then "obj_" ++ vtype ++ "," ++ genInlineFargs'  b ts
                                                  else "obj_" ++ vtype ++ " " ++ vname ++ "," ++ genInlineFargs' b ts


genInlineFargs :: Name -> Bool -> FormArgs -> CCode
genInlineFargs name b l = if b then
                              if null l then "()"
                               else "(" ++ init (genInlineFargs'  (not b) l) ++ ")"
                             else
                               let args = genInlineFargs'  b l in 
                                 if null args then "(" ++ "obj_" ++ name ++ " this" ++ ")" 
                                  else  "(" ++ "obj_" ++ name ++ " this," ++ args ++ ")"



genMethodList :: CodeGenData  -> [Method] -> MCodeGen CodeGenData
genMethodList code_gen_data = foldM (\old_data m -> do      -- dot or dollar?
                               new_data <- genMethod old_data m
                               return $ new_data{code=unlines [code old_data, code new_data]}) code_gen_data{code=""}

genMethod :: CodeGenData  -> Method -> MCodeGen CodeGenData
genMethod code_gen_data mthd@(Method name mname fargs mrety stmtblock)= do
                                              -- form_args_data <- genFormArgs code_gen_data False fargs
                                              let form_args = genInlineFargs name False fargs 
                                              let tempStr = form_args
                                              let ty = fromJust mrety
                                              lvars_data <- genLocalVars code_gen_data{code="", methd_rety=mrety} mthd
                                              stmb_data  <- genStmtBlock lvars_data{code=""} stmtblock
                                              let new_data = unlines ["obj_"
                                                                      ++ ty ++ " "
                                                                      ++ name ++ "_method_"
                                                                      ++ mname ++ tempStr ++ "{\n"
                                                                       , code lvars_data
                                                                       , code stmb_data
                                                                       , "\n}"
                                                                       ]
                                              put stmb_data{code=new_data}
                                              return stmb_data{code=new_data}

genLocalVars :: CodeGenData -> Method -> MCodeGen CodeGenData
genLocalVars code_gen_data (Method _ _ _ _ stmtblock) = genVar code_gen_data stmtblock

genVar :: CodeGenData -> StmtBlock  -> MCodeGen CodeGenData
genVar code_gen_data = foldM genVarS code_gen_data

genVarS :: CodeGenData -> Statement  -> MCodeGen CodeGenData
genVarS code_gen_data (Assignment (Lid varname) _ rexpr)  = do
                                        let varstr_name = "lvar_" ++ varname
                                        let ty = maybe (error (varname ++ "not in context!")) id $ M.lookup varname (tyctx code_gen_data)   --- fix this!
                                        let varstr = "obj_" ++ (getTypeName ty) ++ " " ++ varstr_name ++ ";\n"
                                        let new_lvars = if (varstr_name, getTypeName ty) `elem` (localvars code_gen_data)
                                                             then (localvars code_gen_data) else (varstr_name, getTypeName ty) : (localvars code_gen_data)
                                        return code_gen_data{localvars=new_lvars, code=varstr}
genVarS code_gen_data              _                                  = return code_gen_data


genMethodSigList :: CodeGenData  -> [Method] -> MCodeGen CodeGenData
genMethodSigList  code_gen_data = foldM (\old_data m -> do
                                          new_data <- genMethodSig old_data m
                                          return new_data{code=unlines [code old_data, code new_data]}) code_gen_data{code=""}

genMethodSig :: CodeGenData  -> Method -> MCodeGen CodeGenData
genMethodSig code_gen_data (Method class_name mname fargs mrety _) = do
                                                  let returnTy = maybe "" id mrety
                                                  let new_code = "obj_" ++ returnTy ++ "(* " ++ mname ++ ") " ++ genInlineFargs class_name True fargs ++ ";"
                                                  return code_gen_data{code=new_code}

genMethodObjList' :: CodeGenData  -> [Method] -> CCode
genMethodObjList' code_gen_data = foldl (\str m -> unlines [str, genMethodObj code_gen_data{code=""} m]) "" -- check this out!

genMethodObjList :: CodeGenData -> [Method] -> MCodeGen CodeGenData
genMethodObjList code_gen_data  l = do
                              let cl_name = case class_name code_gen_data of
                                                 Nothing -> ""
                                                 Just nm -> nm
                              let superclass = super code_gen_data
                              let new_code   =   unlines [
                                                    "struct class_" ++ cl_name ++ "_struct " ++ "the_class_" ++ cl_name ++ "_struct = \n{"
                                                    , "&the_class_" ++ superclass ++ "_struct,"
                                                    , "new_" ++ cl_name ++ ","
                                                    , if null l then "" else init (genMethodObjList' code_gen_data l)
                                                    , "\n};"
                                                    ]
                              return code_gen_data{code=new_code}

genMethodObj :: CodeGenData  -> Method -> CCode
genMethodObj code_gen_data (Method class_name mname _ mrety _) =  let returnTy = maybe "" id mrety in
                                                class_name ++ "_method_" ++ mname ++ ","



genStmt :: CodeGenData  -> Statement  -> MCodeGen CodeGenData
genStmt code_gen_data   (Assignment lexpr mstr rexpr) = do
                  case mstr of
                      Nothing  -> case lexpr of
                        Lid varname -> case  M.lookup varname (tyctx code_gen_data) of
                                Nothing -> return code_gen_data{code="variable" ++ varname ++ "not in context"}
                                Just ty -> do
                                  rexpr_data <- genRExpr code_gen_data rexpr
                                  let to_assign = if null (varstack rexpr_data) then code rexpr_data else fst $ head (varstack rexpr_data)
                                  let new_data = if (varname, (getTypeName ty)) `elem` (assigned rexpr_data) 
                                                      then 
                                                        varname ++ " = " ++ to_assign ++ ";\n"
                                                        else "obj_" ++ (getTypeName ty) ++ " " ++ varname ++ " = " ++ "(obj_" ++ (getTypeName ty) ++ ") " ++ to_assign ++ ";\n"
                                  let new_assigned = if (varname, (getTypeName ty)) `elem` (assigned rexpr_data) 
                                                        then (assigned rexpr_data) else (varname, (getTypeName ty)):(assigned rexpr_data)
                                  return rexpr_data{code=unlines [code rexpr_data ++ ";", new_data], assigned=new_assigned}
                        lobj@(ObjField rexp vname) -> case rexp of
                          LExpr (Lid name)  -> do
                            rexpr_data <- genRExpr code_gen_data rexpr
                            let new_code = if (in_constr rexpr_data)
                                                then
                                                 "this -> " ++ vname ++ " = " 
                                                          ++ (fst $ head $ varstack rexpr_data)
                                                          ++ ";\n"
                                                  else
                                                     case (inferLExpr (ctable rexpr_data) (class_name rexpr_data) (cvartable rexpr_data) (tyctx rexpr_data) lobj) of
                                                     Left err -> error (show err)
                                                     Right ty ->
                                                          let to_assign = if null (varstack rexpr_data) then code rexpr_data else fst $ head (varstack rexpr_data) in
                                                          -- "obj_" ++ (getTypeName ty) ++ " " ++ 
                                                          name ++ " -> " ++ vname ++ " = " ++ to_assign ++ ";\n"
                            return rexpr_data{code=unlines [(code rexpr_data) ++ ";", new_code ++ ";"]}
                          _                       -> error "This is not allowed in quack! assigning to complicated rexpr"
                      Just tyname -> case lexpr of
                                    Lid varname           -> do
                                      rexpr_data <- genRExpr code_gen_data rexpr
                                      let to_assign = if null (varstack rexpr_data) then code rexpr_data else fst $ head (varstack rexpr_data)
                                      let new_code = varname ++ " = (obj_" ++ tyname ++ ")" ++ to_assign ++ ";\n"
                                      return rexpr_data{code=new_code}
                                    ObjField rexp vname   -> error "undefined"
genStmt code_gen_data   (StatementR rexpr)                = genRExpr code_gen_data rexpr >>= \new_data -> return new_data{code=(code new_data) ++ ";"}
genStmt code_gen_data   (If (rexpr, smtb) stmt_list mstmblock) = do
                                                      let new_c = (count_if code_gen_data)
                                                      let c = new_c + 1
                                                      let t_label = true_label (genIfLabel c)
                                                      let f_label = false_label (genIfLabel c)
                                                      let e_label = end_label (genIfLabel c)
                                                      let ol_data = code_gen_data
                                                      rexpr_data  <- genRExpr code_gen_data  rexpr 
                                                      stmt_l_data <- genStmtBlock rexpr_data{code=""} stmt_list 
                                                      smtb_data   <- genStmtBlock stmt_l_data{code=""} smtb
                                                      mystr <- case mstmblock of
                                                                          Nothing -> return ""
                                                                          Just block -> do
                                                                            block_data <- genStmtBlock smtb_data block
                                                                            return $ code block_data
                                                      let new_code = unlines [
                                                                        code rexpr_data ++ ";"
                                                                      , "if (" ++ (fst $ head $ varstack rexpr_data) ++ " == " ++ "lit_true" ++ ") {"
                                                                      , "goto " ++ t_label ++ ";"
                                                                      , "}"
                                                                      , "goto " ++ f_label ++ ";"
                                                                      , t_label ++ ":;"
                                                                      , "\t" ++ code smtb_data ++ ";"
                                                                      , "goto " ++ e_label ++ ";"
                                                                      , f_label ++ ":;"
                                                                      , "\t" ++ code stmt_l_data ++ ";"
                                                                      , "\t" ++ mystr ++ ";"
                                                                      , e_label ++ ":;\n"
                                                                               ]
                                                      return code_gen_data{code=new_code, count_if= 1 + count_if smtb_data}
genStmt code_gen_data   (While (rexpr, smstmt))                 = do
                                                            let new_c = (count_while code_gen_data)
                                                            let c = new_c + 1
                                                            let a_label = again_label (genWhileLabel c)
                                                            let t_label = test_label (genWhileLabel c)
                                                            rexpr_data <- genStmt code_gen_data (If(rexpr, smstmt) [] (Nothing))
                                                            loop_data  <- genStmtBlock rexpr_data{varstack=varstack rexpr_data ++ (varstack code_gen_data), code="", count_while=(count_while rexpr_data) + 1} smstmt
                                                            let new_code = unlines [
                                                                                        t_label ++ ":;"
                                                                                      , code rexpr_data ++ ";"
                                                                                      , "goto " ++ a_label ++ ";"
                                                                                      , a_label ++ ":;"
                                                                                      , code loop_data ++ ";"
                                                                                    ]
                                                            return loop_data{code=new_code}

genStmt code_gen_data   (Return mrexpr)                        = case mrexpr of
                                                        Nothing -> return code_gen_data{code="return;"}
                                                        Just r  -> case (methd_rety code_gen_data) of
                                                          Nothing -> do
                                                            rexpr_data <- genRExpr code_gen_data  r
                                                            let temp = if null (varstack rexpr_data) then "" else fst $ head $ varstack rexpr_data
                                                            let new_code = unlines [
                                                                                  code rexpr_data
                                                                                , "return (obj_Nothing)" ++ temp ++ ";"
                                                                                  ]
                                                            return rexpr_data{code=new_code}
                                                          Just t  -> do
                                                            rexpr_data <- genRExpr code_gen_data  r
                                                            let to_assign = if null (varstack rexpr_data) then "" else fst $ head (varstack rexpr_data)
                                                            let new_code = unlines [
                                                                                  code rexpr_data
                                                                                , "return (obj_" ++ t ++ ")" ++ to_assign ++ ";"
                                                                                  ]
                                                            return rexpr_data{code=new_code}
genStmt code_gen_data  (TyCase rexpr ty_altl)                 = return code_gen_data{code="undefined"}



genTyAltList :: CodeGenData -> [TyAlt] -> CCode
genTyAltList code_gen_data ty_alt_l= "undefined"

genTyAlt :: CodeGenData  -> TyAlt -> CCode
genTyAlt code_gen_data talt = "undefined"


genStmtBlock :: CodeGenData  -> StmtBlock -> MCodeGen CodeGenData
genStmtBlock code_gen_data = foldM (\old_data stmt1 -> do
                                   new_data <- genStmt old_data stmt1
                                   return $ new_data{code=unlines [
                                    --  code old_data, 
                                   code new_data], varstack=varstack old_data ++ (varstack new_data)}) code_gen_data


genLExpr :: CodeGenData  -> LExpr -> MCodeGen CodeGenData
genLExpr code_gen_data (Lid str) = return code_gen_data{code=str ++ " "} -- add str to varstack???
genLExpr code_gen_data (ObjField rexpr str) = case rexpr of
                                                   LExpr (Lid "this") -> do
                                                     let new_code = "this -> " ++ str
                                                     return code_gen_data{code=new_code}
                                                   LExpr (Lid varname) -> case M.lookup varname (tyctx code_gen_data) of
                                                                                     Nothing -> do 
                                                                                           let (name, ty) = if null (localvars code_gen_data) 
                                                                                                             then error ("to_debug -> " ++ show (localvars code_gen_data))
                                                                                                                else head (localvars code_gen_data)
                                                                                           let new_code = name ++ " -> " ++ str 
                                                                                           return code_gen_data{code=new_code}
                                                                                     Just t    -> return code_gen_data{code=varname ++ " -> " ++ str, localvars=(varname ++ " -> " ++ str, getTypeName t):(localvars code_gen_data)}
                                                   re         -> do
                                                     rexpr_data <- genRExpr code_gen_data re
                                                     let new_code = code rexpr_data ++ "->" ++ str
                                                     return rexpr_data{code=new_code}

genRExpr :: CodeGenData  -> RExpr -> MCodeGen CodeGenData
genRExpr code_gen_data  (Neg rexpr)               = return code_gen_data{code="undefined"}
genRExpr code_gen_data  (Str str)                 = do
                                                let vars = (varstack code_gen_data)
                                                let new_count = 1 + length vars
                                                let new_temp = "temp" ++ show new_count 
                                                let new_vars = (new_temp, "String") : vars 
                                                let new_code = "obj_String temp" ++ show new_count ++ " = str_literal(\"" ++ str ++ "\");\n"
                                                return code_gen_data{code=new_code, varstack=new_vars}
genRExpr code_gen_data  (Bool b)                  = do 
                                                let vars = (varstack code_gen_data)
                                                let new_count = 1 + length vars
                                                let new_temp = "temp" ++ show new_count 
                                                let new_vars = (new_temp, "Boolean") : vars 
                                                let new_code = "obj_Boolean temp" ++ show new_count ++ " = " ++ if b then "lit_true" else "lit_false"
                                                return code_gen_data{code=new_code, varstack=new_vars}
genRExpr code_gen_data  (P.Int n)                   = do 
                                                let vars = (varstack code_gen_data)
                                                let new_count = 1 + length vars
                                                let new_temp = "temp" ++ show new_count 
                                                let new_vars = (new_temp, "Int") : vars 
                                                let new_code = "obj_Int temp" ++ show new_count ++ " = int_literal(" ++ show n ++ ");\n"
                                                return code_gen_data{code=new_code, varstack=new_vars}
genRExpr code_gen_data  (LExpr lexpr)             = do 
                                                case lexpr of 
                                                   Lid varname -> case M.lookup varname (tyctx code_gen_data) of 
                                                                       Nothing -> do  
                                                                           let new_code = let stack = (varstack code_gen_data) in if null  stack then "// to_debug -> varstack: []" else fst $ head stack 
                                                                           return code_gen_data{code=new_code}
                                                                       Just t  -> do 
                                                                              let new_code = let stack = (localvars code_gen_data) in if null stack then "// to_debug -> localvars: []" else fst $ head stack
                                                                              return code_gen_data{code=new_code}
                                                   re  -> do   
                                                           new_data_c <- genLExpr code_gen_data re 
                                                           let new_code = let stack = (varstack new_data_c) 
                                                                               in if null stack 
                                                                                  then let 
                                                                                    lvar_stack = (localvars new_data_c) in 
                                                                                      if null lvar_stack then "// to_debug -> varstack: []"
                                                                                      else fst $ head lvar_stack
                                                                                   else fst $ head stack
                                                           return new_data_c{code=new_code}                                           
genRExpr code_gen_data  (AND rexp1 rexp2)         = return code_gen_data{code="undefined"}
genRExpr code_gen_data  (OR  rexp1 rexp2)         = return code_gen_data{code="undefined"}
genRExpr code_gen_data  (NOT rexpr)               = return code_gen_data{code="undefined"}
genRExpr code_gen_data  curr_exp@(RArgs rexpr str actargs) = do
                                                case actargs of 
                                                  Nothing -> do 
                                                    new_rexp_c <- genRExpr code_gen_data rexpr
                                                    let new_code = code new_rexp_c 
                                                    let (tempname, tempty) = if null (varstack new_rexp_c) then ("", "") else head (varstack new_rexp_c)
                                                    case rexpr of 
                                                      LExpr (Lid varname) -> do 
                                                                let new_mtd_t = genMethodTable (ctable new_rexp_c)
                                                                case M.lookup tempty (ctable code_gen_data) of 
                                                                  Nothing     -> error ("// to_debug -> " ++ show tempty ++ "is not in class table! called by rargs")
                                                                  Just mtable ->  do 
                                                                      let to_last = let sig = findMethodSig (str, tempty) new_mtd_t 
                                                                                      in case (makeQuackTypeFromStr tempty (ctable new_rexp_c)) of 
                                                                                        Nothing -> error ("// to_debug -> no class " ++ tempty ++ "found!")
                                                                                        Just ty -> case findMethodOfClass str sig  ty (ctable new_rexp_c) of 
                                                                                                  Nothing     -> []
                                                                                                  Just (t, l) -> l 
                                                                      let ret_ty = last $ to_last 
                                                                      let fin_code = unlines [
                                                                                               new_code,
                                                                                              "obj_" ++ (getTypeName ret_ty) ++ " temp" ++ show (1 + length (varstack new_rexp_c)) ++ " = " ++ "(obj_" ++ (getTypeName ret_ty) ++ ") (" ++ varname ++ " -> clazz -> " ++ str ++ "((obj_" ++ tempty ++ ") " ++ varname ++ "));\n"]
                                                                      return new_rexp_c{code=fin_code, varstack=(("temp" ++ show (1 + length (varstack new_rexp_c))), (getTypeName ret_ty)):(varstack new_rexp_c)}
                                                      _ -> do 
                                                            let new_mtd_t = genMethodTable (ctable new_rexp_c)
                                                            let curr_data = new_rexp_c{method_sig_t=new_mtd_t}
                                                            --findMethodOfClass method_name meth_sig qtype ctable
                                                            -- findMethodOfClass str () tempty (ctable new_rexp_c)
                                                            --let to_last = snd $ maybe (Bottom, [])id $ M.lookup (str, tempty) (method_sig_t curr_data)
                                                            let (to_last, s) = let sig = findMethodSig (str, tempty) new_mtd_t 
                                                                              in case (makeQuackTypeFromStr tempty (ctable new_rexp_c)) of 
                                                                                Nothing -> error ("// to_debug -> no class " ++ tempty ++ "found!")
                                                                                Just ty -> case findMethodOfClass str sig  ty (ctable new_rexp_c) of 
                                                                                          Nothing     -> ([], sig)
                                                                                          Just (t, l) -> (l, sig) --case M.lookup (str, tempty) (method_sig_t curr_data) of 
                                                              
                                                            let curr_t = if null to_last then error ("empty type signature for method " ++ str 
                                                                                            ++ "\n" ++ show s) else last to_last
                                                            let fin_code = unlines [
                                                                                  new_code ++ ";"
                                                                                , "obj_" ++ (getTypeName curr_t) ++ " temp" ++ show (1 + length (varstack curr_data)) ++ " = " ++ "(obj_" ++ (getTypeName curr_t) ++ ") (" ++ tempname ++ " -> clazz -> " ++ str ++ "((obj_" ++ tempty ++ ") " -- ++ exp_name 
                                                                                        ++ tempname ++ "))"
                                                                                , "/*" ++ show (str, tempty)
                                                                                -- , show (varstack new_rexp_c)
                                                                                -- ,show (length (varstack new_rexp_c))
                                                                                , show $ varstack curr_data 
                                                                                , show $ varstack new_rexp_c
                                                                                , "hello world"
                                                                                , "*/"
                                                                                ]
                                                            return new_rexp_c {code=fin_code}    -- change keep like this for now!
                                                  Just rexpr_l  -> do 
                                                    new_rexp_c <- genRExpr code_gen_data rexpr   -- new_gen_data for code_gen_data??
                                                    new_gen_data <- genRExprLT new_rexp_c rexpr_l
                                                    let act_code = code new_gen_data
                                                    let new_code = code new_rexp_c
                                                    let (tempname, tempty) = if null (varstack new_gen_data) then 
                                                                                   if null (localvars new_gen_data) then ("", "") 
                                                                                    else head (localvars new_gen_data)
                                                                                    else head (varstack new_gen_data)
                                                    -- let exp_name = if null (varstack new_rexp_c) then "empty_name" else fst $ head $ varstack new_rexp_c
                                                    let new_mtd_t = genMethodTable (ctable new_gen_data)
                                                    let curr_data = new_gen_data{method_sig_t=new_mtd_t}
                                                    -- let myty = (case class_name curr_data of 
                                                    --                                     Nothing -> "Obj"
                                                    --                                     Just t  -> t)
                                                    let (to_last, s) = let sig = findMethodSig (str, tempty) new_mtd_t 
                                                                              in case makeQuackTypeFromStr tempty (ctable curr_data) of 
                                                                                Nothing -> error ("// to_debug -> no class " ++ tempty ++ "found for: " 
                                                                                                    ++ str ++ " " ++ (show tempty) 
                                                                                                    ++ "\n" ++ show (varstack new_rexp_c)
                                                                                                    ++ "\n" ++ show (varstack new_gen_data)
                                                                                                    ++ "\n" ++ show rexpr_l
                                                                                                    ++ "\n" ++ show curr_exp
                                                                                                    ++ "\n" ++ show (localvars new_gen_data))
                                                                                Just ty -> case findMethodOfClass str sig  ty (ctable curr_data) of 
                                                                                          Nothing     -> ([Obj], sig)
                                                                                          Just (_, l) -> (l, sig)
                                                    let curr_t = if null to_last then error "this implementatation is wrong!" else last to_last -- wrong
                                                    let fin_code = unlines [
                                                                            -- new_code ++ ";",
                                                                          act_code, 
                                                                          "obj_" ++ (getTypeName curr_t) ++ " temp" ++ show (1 + length (varstack curr_data)) ++ " = " ++ "(obj_" ++ (getTypeName curr_t) ++ ")(" ++ tempname ++ " -> clazz -> " ++ str ++ "((obj_" ++ tempty ++ ") " -- ++ exp_name 
                                                                                  ++ (if null (varstack curr_data) then "" else init $ init (concatMap (++ ", ") (take (length $ init to_last) $ fst $ unzip $ varstack curr_data))) ++ "))"
                                                                          , "/*" ++ show (str, tempty)
                                                                          -- , show (varstack new_rexp_c)
                                                                          -- ,show (length (varstack new_rexp_c))
                                                                          , show $ varstack curr_data 
                                                                          , show $ varstack new_gen_data

                                                                          , "*/"
                                                                          ]
                                                    return curr_data{code=fin_code, varstack=("temp" ++ show (1 + length (varstack curr_data)), getTypeName curr_t):(varstack curr_data)}

genRExpr code_gen_data  (Constr str actargs)      = do
                                                case actargs of 
                                                  Nothing -> do 
                                                        let fin_code = "obj_" ++ str ++ " temp" ++ show (1 + length (varstack code_gen_data)) ++ " = " ++ "(obj_" ++ str ++ ")" ++ "new_" ++ str ++ "();\n"--unlines [new_code, "obj_" ++ str ++ " temp" ++ show (1 + length (varstack code_gen_data)) ++ " = " ++  "the_class_" ++ str ++ " -> clazz -> constructor(" ++ myty ++ ") " ++ varname ++ ");\n"]
                                                        return code_gen_data{code=fin_code, varstack=(("temp" ++ show (1 + length (varstack code_gen_data))), str):(varstack code_gen_data)}
                                                  Just rexpr_l  -> do 
                                                    new_gen_data <- genRExprLT code_gen_data rexpr_l
                                                    let act_code = code new_gen_data
                                                    let exp_name = "the_class_" ++ str 
                                                    let curr_t    = str 
                                                    let fin_code = unlines [
                                                                            act_code
                                                                          -- , new_code
                                                                          , "obj_" ++ curr_t ++ " temp" ++ show (1 + length (varstack new_gen_data)) ++ " = "  ++ "(obj_" ++ curr_t ++ ")" ++ exp_name ++ " -> clazz -> constructor("
                                                                                  ++ (if null (varstack new_gen_data) then "" else init $ init (concatMap (++ ", ") (fst $ unzip $ varstack new_gen_data))) ++ ")"
                                                                          ]
                                                    return new_gen_data{code=fin_code, varstack=("temp" ++ (show (1 + length (varstack new_gen_data))), str):(varstack new_gen_data)}




-- popTemps  ::
genBranch :: Int -> String -> [(String, String)] -> String
genBranch count stmt [(truepart, truecode), (falsepart, falsecode)] = unlines [
                                                                 truepart ++ ":;\n"
                                                                , truecode
                                                                , "goto end_b" ++ show (count + 1)
                                                                , falsepart ++ ":;\n"
                                                                , falsecode
                                                                , "end_b" ++ show (count + 1)
                                                              ]

genActArgsRexp' :: [String] -> CodeGenData -> RExpr -> String -> MCodeGen CodeGenData
genActArgsRexp' temps code_gen_data rexp type_n = do
                                           new_data <- genRExpr code_gen_data rexp
                                           let rcode = code new_data
                                           let to_return = "obj_" ++ type_n  ++ (" temp" ++ show (length temps)) ++ " = " ++ rcode ++ ";\n"
                                           return new_data{code=to_return, varstack=("temp" ++ show (length temps), type_n): varstack new_data}


genActArgs'' :: Name -> Name -> [String] -> CodeGenData -> ActArgs -> (String, [String]) -> MCodeGen CodeGenData
genActArgs'' varname m_name temps code_gen_data Nothing (t, lT)     = do
                                                                  let new_code = "obj_"
                                                                        ++ t  --show lT--(last lT) -- probably wrong
                                                                        ++ " temp" ++ show (length temps + 1)
                                                                        ++ " = " ++ varname ++ " -> clazz -> " ++ m_name ++ "( "
                                                                        ++"(obj_" ++ t ++ ") this, " ++ (if null temps then "" else init $ concatMap (++ ",") temps) ++ ");\n"
                                                                  let new_var = (" temp" ++ show (length temps + 1), last lT) : (zip temps lT)
                                                                  return code_gen_data{code=new_code, varstack=new_var}
genActArgs'' varname m_name temps code_gen_data (Just rexplist) (t, tylist) = case zip rexplist tylist of
                                                         []              -> genActArgs'' varname m_name temps code_gen_data Nothing (t, tylist)
                                                         (r, t):rsts     -> do
                                                           new_data   <- genActArgsRexp' temps code_gen_data r t
                                                           let rcode     = code new_data
                                                           put new_data{code=rcode}
                                                           let new_temps = fst $ unzip $ varstack new_data
                                                           let (rs, ts) = unzip rsts
                                                           latest_data <- genActArgs'' varname m_name new_temps new_data (Just rs) (t, ts)
                                                           let mcode        = code latest_data
                                                           put latest_data{code=mcode}
                                                           let new_temps_2 = varstack latest_data
                                                           put latest_data{varstack=new_temps_2}
                                                           let fin_code    = unlines [rcode, mcode]
                                                           put latest_data{code=fin_code}
                                                           return latest_data -- fin_code includes rcode on everysingle turn???




















genTemps :: Int -> [String] -> CCode
genTemps n l = concat (reverse (map (++ ",") (popTemps n l)))


popTemps :: Int -> [String] -> [String]
popTemps n l = popTemps' ((length l) - n) (length l) [] l

popTemps' :: Int -> Int -> [String] -> [String] -> [String]
popTemps' _ _ _ []                   = []
popTemps' n m   to_return l@(x:xs)   =  if n == m then to_return
                          else popTemps' n (m - 1) (x:to_return) xs


-- bool is flag to indicate whether in class constr or not! should probably make it global but ...
genFormArgs :: CodeGenData  -> Bool -> FormArgs -> MCodeGen CodeGenData
genFormArgs code_gen_data b = foldM (\old_data (TyAssign (varname, types)) -> do 
                                          let to_return = if b then "obj_" ++ types ++ " " ++ varname ++ ";\n" -- ++ (code old_data)
                                                             else "obj_" ++ types ++ " " ++ varname ++ "," -- ++ (code old_data)
                                          let new_stack = (varname, types):(localvars old_data)
                                          return old_data{code=to_return, localvars=new_stack}) code_gen_data

genRExprLT :: CodeGenData -> [RExpr] -> MCodeGen CodeGenData
genRExprLT code_gen_data = foldM (\(old_data) rexp -> do 
                              new_data <- genRExpr old_data rexp 
                              let old_code  = code old_data
                              case rexp of 
                                LExpr (Lid varname) -> case M.lookup varname (tyctx old_data) of 
                                  Nothing -> do 
                                    let to_add = if null (varstack old_data) then  ("", "") else head (varstack old_data)
                                    return (new_data{varstack=to_add:(varstack new_data)})        
                                  Just t  -> do
                                    let to_add = if null (localvars old_data) then  ("", "") else head (localvars old_data)
                                    return (new_data{localvars=to_add:(localvars new_data)})  
                                re         -> do
                                    rexpr_data <- genRExpr old_data re
                                    let to_add = if null (varstack old_data) then  ("", "") else head (varstack old_data)
                                    return new_data{varstack=(varstack rexpr_data)}
                              ) code_gen_data

findMethodSig :: (String, String) -> MethodT -> (QuackType, [QuackType])
findMethodSig (methName, cl_name) methdTable = case M.toList methdTable of 
                                   []                  -> (Bottom, [])
                                   ((m, cl), ans):mscls -> if m == methName && cl == cl_name 
                                                       then ans else findMethodSig (methName, cl) (M.fromList mscls)
                                    