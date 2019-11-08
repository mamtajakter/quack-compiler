{-# LANGUAGE GADTs #-}

module TypeChecker.TypeCheck where

import Parser.QuackAST
import qualified Parser.QuackAST as P
import TypeChecker.QuackTypes
import qualified TypeChecker.QuackTypes as T
import qualified Data.Map as M
import Data.Maybe



data TypeError where
  Unbound            :: String   -> TypeError
  UninitTy           :: String   -> TypeError
  Uninfer            :: String   -> TypeError
  AlreadyDefTy       :: String   -> TypeError
  TyError            :: String   -> TypeError
  IfTyError          :: StmtBlock -> StmtBlock  -> TypeError
  CastErr            :: QuackType -> QuackType -> TypeError
  FieldError         :: String   -> TypeError
  ConstrErr          :: String   -> TypeError
  StmtTyError        :: Statement -> QuackType -> QuackType -> TypeError -- change prog to corresponding child of
  RExprTyError       :: RExpr     -> QuackType -> QuackType -> TypeError
  LExprTyError       :: LExpr     -> QuackType -> QuackType -> TypeError
  ArgsTyErr          :: String    -> TypeError
  MethodMisTyErr     :: String    -> String    -> TypeError
  RExprListTyErr     :: [RExpr]   -> [QuackType]  -> TypeError
  RExprListElemTyErr :: RExpr -> QuackType -> [RExpr] -> QuackType -> TypeError
  RExprListUninf     :: RExpr -> [RExpr]     -> TypeError
  ReturnTyError      :: RExpr -> QuackType -> QuackType -> TypeError
  InformError        :: String -> TypeError


instance Show TypeError where
  show = showTyError

showTyError :: TypeError -> String
showTyError (Unbound x)                 = "Unbound variable " ++ show x ++ " has no type"
showTyError (StmtTyError stmt t1 t2)    = "Statement " ++ show stmt ++ "should be of type "
                                        ++ show t1 ++ "But is of type " ++ show t2
showTyError (RExprTyError rexpr t1 t2)  = unlines ["Right hand expression ", show rexpr
                                        , "should be of type ", show t1, " but is of type ", show t2]
showTyError (LExprTyError lexpr t1 t2)  = unlines ["Left hand expression ", show lexpr
                                        , " should be of type ", show t1,  " but is of type ", show t2]
showTyError (ConstrErr str)             = "Constructor of class " ++ show str ++ " undefined!"
showTyError (ArgsTyErr str)             = "Defined arguments for " ++ show str ++ "don't match used arguments!"
showTyError (MethodMisTyErr str1 str2)  = unlines ["Type ", show str1, "has no method definition of ", show str2]
showTyError (RExprListTyErr rlist  qlist2) = unlines ["List ", show rlist, "cannot possibly have type ", show qlist2]
showTyError (RExprListElemTyErr r q rlist ty)    = unlines ["Expression ", show r, "in list", show rlist, "should be of type", show q, "but is of type", show ty]
showTyError (RExprListUninf r rexplist)          = unlines ["The type of expression: ", show r, "in list", show rexplist, "could not be inferred!"]
showTyError (UninitTy cls)   = "Uninitialize type" ++ cls
showTyError (TyError s)      = show s ++ " should not be in AST!"
showTyError (InformError s)   = s
showTyError (FieldError s)   = s ++ "is not a field of the given class!"
showTyError (CastErr ty1 ty2) = "Unable to downcast " ++ show ty2 ++ "into " ++ show ty1
showTyError (IfTyError b1 b2) = "The types in blocks" ++ show b1 ++ " " ++ show b2 ++ " do not match!"
showTyError (AlreadyDefTy name) = "Class " ++ name ++ "is already defined!"
showTyError (ReturnTyError rexp ty1 ty2) = "Expression " ++ show rexp ++ " should have type" ++ show ty1 ++ " but is of type " ++ show ty2


typeCheckProg :: Prog -> Either TypeError (CTable, CVarTable, TyCtx)
typeCheckProg = checkProg initCTable M.empty M.empty

-- build method table with all the methods defined in ctable and make it exposed. This allows for method dispatch
-- and method type checking!

-- refactor to use record to hold ctable, cvartable, and tyctx

checkProg :: CTable -> CVarTable -> TyCtx -> Prog -> Either TypeError (CTable, CVarTable, TyCtx)
checkProg ctable cvartable ctx (Prog cl_list st_list) = checkClassList ctable cvartable ctx cl_list
                                                      >>= \(clt, cvt, tyctx) -> checkStmtBlock Nothing Nothing clt cvt tyctx st_list
                                                      >>= \(cvt1, tyctx1) -> Right (clt, cvt1, tyctx1)

checkClassList :: CTable -> CVarTable -> TyCtx -> [Class] -> Either TypeError (CTable, CVarTable, TyCtx)
checkClassList ctable cvartable ctx []     = Right (ctable, cvartable, ctx)
checkClassList ctable cvartable ctx (c:cs) = checkClass ctable cvartable ctx c
                                           >>= \(clt, cvt, tyctx) -> checkClassList clt cvt M.empty cs -- Right ctable

checkClass :: CTable -> CVarTable -> TyCtx -> Class -> Either TypeError (CTable, CVarTable,TyCtx)
checkClass ctable cvartable ctx (Class cl_sig cl_body) = checkClassSig ctable cvartable ctx cl_sig
                                                       >>= \(nm, ct, cvart, new_ctx) -> checkClassBody nm ct cvart new_ctx cl_body
                                                       >>= \(ctab, cvt, c) -> Right (ctab, cvt, c)


checkClassSig :: CTable  -> CVarTable -> TyCtx -> ClassSig -> Either TypeError (Name, CTable, CVarTable, TyCtx)
checkClassSig ctable  cvartable ctx  (ClassSig str fargs Nothing)      = case M.lookup str  ctable of
                                                                            Just table ->  Left (AlreadyDefTy str)
                                                                            Nothing    ->  case inferFormArgs ctable ctx fargs of
                                                                              Left err   -> Left err
                                                                              Right (new_ctx, ty_l) -> let new_cvart = M.insert str (new_ctx, M.empty) cvartable
                                                                                                       in  let pat = UserDfn str Obj
                                                                                                       in Right (str,
                                                                                                       M.insert str (M.fromList [("constructor", (Obj, ty_l ++ [pat]))]) ctable
                                                                                                       , new_cvart, new_ctx)
checkClassSig ctable  cvartable ctx  (ClassSig str fargs (Just exten)) = case M.lookup str  ctable of
                                                                            Just table ->  Left (AlreadyDefTy str)
                                                                            Nothing    ->  case M.lookup exten ctable of
                                                                              Nothing -> Left (Unbound exten)
                                                                              Just table -> case M.lookup "constructor" table of
                                                                                Nothing -> Left (ConstrErr exten)
                                                                                Just (ty, l) -> case inferFormArgs ctable ctx fargs of
                                                                                       Left err  -> Left err
                                                                                       Right (new_ctx, ty_l) -> case M.lookup exten cvartable of
                                                                                                      Nothing             -> let new_cvart       = M.insert exten (M.empty, M.empty) cvartable in
                                                                                                                             let to_return_cvart = M.insert str (new_ctx, M.empty) cvartable in
                                                                                                                             let extenTy = last l in let pat = UserDfn str extenTy in
                                                                                                                             Right (str, M.insert str (M.fromList [("constructor", (extenTy, ty_l ++ [pat]))]) ctable, to_return_cvart, new_ctx) --Right (str, M.insert exten (M.empty, M.empty) cvartable, ) --Left (InformError (exten ++ "has no fields or local variables!"))
                                                                                                      Just (flds, l_vars) -> let new_flds = flds `M.union` new_ctx in
                                                                                                                             let new_cvart = M.insert str (new_flds, l_vars) cvartable in
                                                                                                                             let extenTy = last l in let pat = UserDfn str extenTy in  Right (str,
                                                                                                                             M.insert str (M.fromList [("constructor", (extenTy, ty_l ++ [pat]))]) ctable
                                                                                                                             , new_cvart, new_ctx)

checkClassBody :: Name -> CTable -> CVarTable -> TyCtx -> ClassBody -> Either TypeError (CTable, CVarTable, TyCtx)
checkClassBody cl_name ctable cvartable ctxt (ClassBody st_list meth_list) = let meth_ctxts = genEmptyCtxs meth_list in
                                                                              checkStmtBlock Nothing (Just cl_name) ctable cvartable ctxt st_list -- ctxt = local vars of class  -- needs fixing sure! check all methods together including constructor.
                                                                              >>= \(cvt, ct) -> checkMethodList cl_name ctable cvt ct meth_ctxts meth_list
                                                                                            -- >> Right () -- don't need it?



genEmptyCtxs :: [a] -> [TyCtx]
genEmptyCtxs = map (const M.empty)

checkMethodList :: Name -> CTable -> CVarTable -> TyCtx -> [TyCtx] -> [Method] -> Either TypeError (CTable, CVarTable, TyCtx)
checkMethodList cl_name ctable cvartable outctx ctxs mthds = case zip ctxs mthds of
                              []           -> Right (ctable, cvartable, outctx)
                              (c,x):csxs   ->
                                         case M.lookup cl_name cvartable of
                                           Nothing           -> checkMethod ctable cvartable c x
                                                                 >>= \(cvt, ctable1, ct) -> (uncurry (checkMethodList cl_name ctable1 cvt M.empty) (unzip csxs))

                                           Just (flds, l_vars) -> checkMethod ctable cvartable (c `M.union` flds `M.union` l_vars) x
                                                                 >>= \(cvt, ctable1, ct) -> (uncurry (checkMethodList cl_name ctable1 cvt M.empty) (unzip csxs))





checkMethod :: CTable -> CVarTable -> TyCtx -> Method -> Either TypeError (CVarTable, CTable, TyCtx)
checkMethod ctable cvartable ctx (Method cl_name meth_name f_args mret_ty stmtb) -- return type to check !
                                  = case M.lookup cl_name ctable of
                                         Nothing         -> error "method has to be attached to class!"
                                         Just meth_table -> case M.lookup meth_name meth_table of
                                           Just _        -> error "method already defined!"
                                           Nothing       -> case inferFormArgs ctable ctx f_args of
                                                               Left err      -> Left err
                                                               Right (ct, f_args_ty) -> case M.lookup "constructor" meth_table of
                                                                 Nothing           -> Left (ConstrErr cl_name)--do
                                                                 Just (obj, arg_l) -> case mret_ty of
                                                                   Nothing      -> Right (cvartable, M.insert cl_name (M.insert meth_name ((last arg_l), f_args_ty) meth_table) ctable, ct) -- insert formal arguments as well
                                                                   Just ty_name -> case M.lookup ty_name ctable of
                                                                     Nothing           -> Left (ConstrErr ty_name)
                                                                     Just nm_meth_list -> case M.lookup "constructor" nm_meth_list of
                                                                                              Nothing     -> Left (ConstrErr ty_name)
                                                                                              Just (t, l) -> case checkStmtBlock (Just (last l)) (Just cl_name) ctable cvartable ct stmtb of
                                                                                                Left err -> Left err
                                                                                                Right _  -> Right (cvartable, M.insert cl_name (M.insert meth_name ((last arg_l), f_args_ty ++ [t]) meth_table) ctable, ct)

                                                                 -- M.insert meth_name obj:f_args ++ [m_ret_ty] meth_table --undefined -- put name of method and arg types in ctable, put local args in ctx, not in world.



-- rewrite with do and records!??
checkStmt :: Maybe QuackType -> Maybe String -> CTable -> CVarTable -> TyCtx -> Statement -> Either TypeError (CVarTable, TyCtx)
checkStmt mfn_ret_ty mcl_name ctable cvartable ctxt (Assignment lexpr Nothing rexpr) = inferRExpr ctable mcl_name cvartable ctxt rexpr
                                                                                   >>= \ty -> case lexpr of
                                                                                     Lid str           -> Right (cvartable, M.insert str ty ctxt)
                                                                                     ObjField rexp str -> case rexp of
                                                                                       LExpr (Lid "this") -> case mcl_name of
                                                                                                  Nothing   -> Left (InformError " 'this' is only defined inside a class definition")
                                                                                                  Just name -> case M.lookup name cvartable of
                                                                                                    Nothing             -> Right (M.insert name (M.empty, M.fromList [(str, ty)]) cvartable, ctxt)
                                                                                                    Just (flds, l_vars) -> let new_l_vars = M.insert str ty l_vars in
                                                                                                                             Right (M.insert name (flds, new_l_vars) cvartable, ctxt)
                                                                                       _                  -> Left (FieldError "only 'this' can access fields ")
checkStmt mfn_ret_ty  mcl_name ctable cvartable ctxt (Assignment lexpr (Just ty) rexpr) = inferRExpr ctable mcl_name cvartable ctxt rexpr
                                                                                      >>= \rty -> case M.lookup ty ctable of
                                                                                        Nothing        -> Left (Unbound ty)
                                                                                        Just mth_table -> case M.lookup "constructor" mth_table of
                                                                                          Nothing                 -> Left (ConstrErr ty)
                                                                                          Just (exten_ty, t_list) -> let my_type = last t_list in if isSubTypeOf rty my_type -- or other way around!
                                                                                                                       then case lexpr of
                                                                                                                         Lid str          -> Right (cvartable, M.insert str my_type ctxt)
                                                                                                                         ObjField rexp str -> case rexp of
                                                                                                                                LExpr (Lid "this") -> case mcl_name of
                                                                                                                                                       Nothing   -> Left (InformError " 'this' is only defined inside a class definition")
                                                                                                                                                       Just name -> case M.lookup name cvartable of
                                                                                                                                                         Nothing             -> Left (InformError ("no class " ++ "name"))
                                                                                                                                                         Just (flds, l_vars) -> let new_l_vars = M.insert str (last t_list) flds in
                                                                                                                                                                                   Right (M.insert name (flds, new_l_vars) cvartable, ctxt)
                                                                                                                                _            -> Left (FieldError "only 'this' can access fields ")
                                                                                                                        else Left (CastErr rty my_type)
                                                                                             -- Left err -> Left err

checkStmt mfn_ret_ty  mcl_name ctable cvartable ctxt (StatementR rexp)  = case inferRExpr ctable mcl_name cvartable ctxt rexp of
                                                                       Left err -> Left err
                                                                       Right ty -> Right (cvartable, ctxt)
checkStmt mfn_ret_ty mcl_name ctable cvartable ctxt (If (rexp, stmtblock) stmt_list Nothing)       = checkRExpr ctable mcl_name cvartable ctxt rexp Boolean
                                                                                        *>  checkStmtBlock mfn_ret_ty mcl_name ctable cvartable ctxt stmtblock
                                                                                        >>= \(cvt, c)  -> checkStmtBlock mfn_ret_ty mcl_name ctable cvt c stmt_list
checkStmt mfn_ret_ty mcl_name ctable cvartable ctxt (If (rexp, stmtblock) stmt_list (Just stmt_b))  = checkRExpr ctable mcl_name cvartable ctxt rexp Boolean
                                                                                         *>  checkStmtBlock mfn_ret_ty mcl_name ctable cvartable ctxt stmtblock
                                                                                         >>= \(cvt, c)  -> checkStmtBlock mfn_ret_ty mcl_name ctable cvt c stmt_list
                                                                                         >>= \(cvt1, c1) -> checkStmtBlock mfn_ret_ty mcl_name ctable cvt1 c1 stmt_b
                                                                                         >>= \(cvt1, tycon) -> if tycon == c && cvt == cvt1 then Right (cvt, tycon) else Left (IfTyError stmtblock stmt_b)
checkStmt mfn_ret_ty mcl_name ctable cvartable ctxt (While (rexp, stmtblock))  = checkRExpr ctable mcl_name cvartable ctxt rexp Boolean
                                                                    *> checkStmtBlock mfn_ret_ty mcl_name ctable cvartable ctxt stmtblock
checkStmt mfn_ret_ty mcl_name ctable cvartable ctxt (Return Nothing)              = Right (cvartable, ctxt)
checkStmt mfn_ret_ty mcl_name ctable cvartable ctxt (Return (Just r))            = case mfn_ret_ty of
                                           Nothing -> inferRExpr ctable mcl_name cvartable ctxt r  >> Right (cvartable, ctxt)
                                           Just ty -> inferRExpr ctable mcl_name cvartable ctxt r >>= \t -> if isSubTypeOf t ty then Right (cvartable, ctxt)  else Left (InformError $ "I fail here: " ++ show (t, ty))--Left (ReturnTyError r t ty)
checkStmt mfn_ret_ty mcl_name ctable cvartable ctxt (TyCase rexp ty_alt_l)                = inferRExpr ctable mcl_name cvartable ctxt rexp >>= \ty -> checkTyAltList ty mfn_ret_ty mcl_name ctable cvartable ctxt ty_alt_l   --fix later!
                                                                    -- ctx <- checkRExpr
      --undefined --inferRExpr ctable ctxt  undefined

checkTyAltList :: QuackType -> Maybe QuackType -> Maybe String -> CTable -> CVarTable -> TyCtx -> [TyAlt] -> Either TypeError (CVarTable, TyCtx)
checkTyAltList supertype mfn_ret_ty mcl_name ctable cvartable ctxt []     = Right (cvartable, ctxt)
checkTyAltList supertype mfn_ret_ty mcl_name ctable cvartable ctxt (t:ts) = checkTyAlt supertype mfn_ret_ty mcl_name ctable cvartable ctxt t
                                                                 >>= \(cvt, c) -> checkTyAltList supertype mfn_ret_ty mcl_name ctable cvt c ts

checkTyAlt :: QuackType -> Maybe QuackType -> Maybe String -> CTable -> CVarTable -> TyCtx -> TyAlt -> Either TypeError (CVarTable, TyCtx)
checkTyAlt supertype mfn_ret_ty mcl_name ctable cvartable ctxt (tyassign, stmtb) = inferTyAssign ctable ctxt tyassign
                                                                        >>= \(c, ty) ->
                                                                             if isSubTypeOf ty supertype
                                                                               then checkStmtBlock mfn_ret_ty mcl_name ctable cvartable c stmtb
                                                                               else Left (CastErr ty supertype)





checkStmtBlock :: Maybe QuackType -> Maybe String -> CTable -> CVarTable -> TyCtx -> StmtBlock -> Either TypeError (CVarTable, TyCtx)
checkStmtBlock mfn_ret_ty mcl_name ctable cvartable ctxt []                 = Right (cvartable, ctxt)
checkStmtBlock mfn_ret_ty mcl_name ctable cvartable ctxt (x:xs)             = checkStmt mfn_ret_ty mcl_name ctable cvartable ctxt x
                                                                   >>= \(cvt, c) -> checkStmtBlock mfn_ret_ty mcl_name ctable cvt c xs



checkLExpr :: CTable -> Maybe Name -> CVarTable -> TyCtx -> LExpr -> Either TypeError ()
checkLExpr ctable cl_name cvartable ctxt (Lid str) = case M.lookup str ctxt of
                   Nothing       -> Left (Unbound str)
                   Just ty       -> Right ()
checkLExpr ctable cl_name cvartable ctxt (ObjField rexp str) =
                                                           case rexp of
                                                             LExpr (Lid "this") ->
                                                                  case cl_name of
                                                                    Nothing -> error "can only have 'this' inside class!"
                                                                    Just name -> case M.lookup name cvartable of
                                                                         Nothing -> Left (Unbound name)
                                                                         Just (flds, l_vars) -> case M.lookup str flds of
                                                                                                    Nothing -> Left (FieldError str)
                                                                                                    Just ty -> Right ()

                                                             _ -> error "only 'this' can access fields!"




-- rewrite with do??
inferRExpr :: CTable -> Maybe Name -> CVarTable -> TyCtx -> RExpr -> Either TypeError QuackType
inferRExpr ctable mname cvartable ctx rexp = case rexp of
  P.Int i            -> Right T.Int
  Str s            -> Right String
  Bool b           -> Right Boolean
  Neg rexp1         -> checkRExpr ctable mname  cvartable ctx rexp1 T.Int  *> Right T.Int
  NOT r            -> checkRExpr ctable mname cvartable ctx r Boolean *> Right Boolean
  AND r1 r2        -> checkRExpr ctable mname cvartable ctx r1 Boolean *> checkRExpr ctable mname cvartable ctx r2 Boolean *> Right Boolean
  OR  r1 r2        -> checkRExpr ctable mname cvartable ctx r1 Boolean *> checkRExpr ctable mname cvartable ctx r2 Boolean *> Right Boolean
  LExpr lexp       -> inferLExpr ctable mname cvartable ctx lexp    -- dynamic dispatch!!!!
  RArgs rexp1 str  Nothing -> case inferRExpr ctable mname cvartable ctx  rexp1 of
               Left err  -> Left err
               Right ty  -> let ty_name = getTypeName ty in
                              case findMethodOfClass str (ty, [ty]) ty ctable of
                                Nothing           -> Left (MethodMisTyErr ty_name str)
                                Just (qt, t_list) -> Right ty
  RArgs rexp1 str  actargs -> case inferRExpr ctable mname cvartable ctx rexp1 of
              Left err -> Left err
              Right ty -> case inferRExprList ctable mname cvartable ctx $ fromJust actargs of
                                    Left err      -> Left err
                                    Right args_ty -> case findMethodOfClass str (ty, args_ty ++ [ty]) ty ctable of
                                                           Nothing           -> let cl_name = (getTypeName ty) in case M.lookup cl_name ctable of
                                                             Nothing             -> Left (Unbound cl_name)
                                                             Just metd_list  -> case M.lookup str metd_list of
                                                               Nothing -> Left (InformError (show (str, ty))) -- str
                                                               Just (t, signature) -> if isListSubTypeOf (ty:args_ty) (init signature) then Right (last signature) else Left (MethodMisTyErr cl_name (show (ty:args_ty, init signature))) --str
                                                           Just (qt, t_list) -> Right $ last t_list
                                                                               -- Left (InformError (show $ t_list))

  Constr str Nothing    -> case M.lookup str ctable of
    Nothing            -> Left (Unbound str)
    Just mthd_list   -> case M.lookup "constructor" mthd_list of
      Nothing              -> Left (ConstrErr str)
      Just (sig, ty_list) -> Right (last ty_list)--let my_type = last ty_list in case findMethodOfClass "constructor" (sig, [my_type]) sig ctable of
                                      --Nothing         -> Left (InformError (show my_type))--Left (ConstrErr str)
                                      --Just (t, tlist) -> Right (last tlist)
  (Constr str actargs)  -> case M.lookup str ctable of
    Nothing             -> Left (Unbound str)
    Just mthd_list     -> case M.lookup "constructor" mthd_list of
      Nothing              -> Left (ConstrErr str)
      Just (sig, ty_list) -> let my_type = last ty_list in case inferRExprList ctable mname cvartable ctx (fromJust actargs) of
                                    Left err      -> Left err
                                    Right args_ty -> case findMethodOfClass "constructor" (sig, args_ty ++ [my_type]) my_type ctable of
                                                          Nothing            -> Left (ConstrErr str)
                                                          Just (t, a_list)  -> Right (last a_list)

  other                          -> Left (TyError (show other))

-- REDO and use foldr
inferRExprList :: CTable -> Maybe Name -> CVarTable -> TyCtx -> [RExpr] -> Either TypeError [QuackType]
inferRExprList ctable mname cvartable ctx []     = Right []
inferRExprList ctable mname cvartable ctx (r:rs) = inferRExpr ctable mname cvartable ctx r
                                 >>= \rty -> inferRExprList ctable mname cvartable ctx rs
                                 >>= \l -> return (rty : l)

inferTyAssign :: CTable -> TyCtx -> TyAssign -> Either TypeError (TyCtx, QuackType)
inferTyAssign ctable ctx (TyAssign (n, ty_name)) = case M.lookup ty_name ctable of
                                  Nothing    -> Left (UninitTy ty_name)
                                  Just meth_list  -> case M.lookup "constructor" meth_list of
                                            Nothing         -> Left (ConstrErr ty_name)
                                            Just (ty, list) -> Right (M.insert n (last list) ctx, ty)

instance Inferable TyAssign where
  inferA = inferTyAssign

instance ListInferable TyAssign

class Inferable a where
  inferA :: CTable -> TyCtx -> a -> Either TypeError (TyCtx, QuackType)


class Inferable a => ListInferable a where
  inferList :: CTable -> TyCtx -> [a] -> Either TypeError [QuackType]
  inferList  ctable ctx []     = Right []
  inferList  ctable ctx (f:fs) = inferA ctable ctx f
                    >>= \(ct, ty) -> inferList ctable ct fs
                    >>= \l  -> return (ty : l)


inferFormArgs :: CTable -> TyCtx -> FormArgs -> Either TypeError (TyCtx, [QuackType])
inferFormArgs ctable ctx []     = Right (ctx, [])
inferFormArgs ctable ctx (f:fs) = inferTyAssign ctable ctx f
                            >>= \(ct, qty) -> inferFormArgs ctable ct fs
                            >>= \(c, l)    -> Right (c, qty : l)

checkRExpr :: CTable -> Maybe Name -> CVarTable -> TyCtx -> RExpr -> QuackType -> Either TypeError ()
checkRExpr ctable mname cvartable ctx rexp ty = case inferRExpr ctable mname cvartable ctx rexp of
  Right ty1 -> if ty == ty1 then Right () else Left (RExprTyError rexp ty ty1)
  Left  err -> Left err

-- inferLExpr :: CTable -> TyCtx -> LExpr -> Either TypeError QuackType
-- inferLExpr = undefined


inferLExpr :: CTable -> Maybe Name -> CVarTable -> TyCtx -> LExpr -> Either TypeError QuackType
inferLExpr ctable cl_name cvartable ctxt (Lid str) = case M.lookup str ctxt of
                   Nothing       -> case cl_name of 
                    Nothing -> Left (Unbound str) -- str
                    Just cl -> case M.lookup cl cvartable of 
                      Nothing            -> Left (Unbound str)
                      Just (flds, lvars) -> case M.lookup str flds of 
                        Nothing -> case M.lookup str lvars of 
                          Nothing -> Left (Unbound str)
                          Just t1 -> Right t1 
                        Just t  -> Right t 
                   Just ty       -> Right ty
inferLExpr ctable cl_name cvartable ctxt (ObjField rexp str) =
                                                           case rexp of
                                                             LExpr (Lid "this") ->
                                                                  case cl_name of
                                                                    Nothing -> Left (InformError "can only have 'this' inside class!")
                                                                    Just name -> case M.lookup name cvartable of
                                                                         Nothing -> Left (Unbound name)
                                                                         Just (flds, l_vars) -> case M.lookup str flds of
                                                                                                    Nothing -> case M.lookup str l_vars of
                                                                                                      Nothing  -> Left (FieldError str) -- str
                                                                                                      Just ty1 -> Right ty1
                                                                                                    Just ty -> Right ty --Left (InformError (show flds))--Right ty
                                                             -- _      -> error "Bosco you need to implement dynamic dispatch!!!!"
                                                             LExpr (Lid other) -> case cl_name of
                                                                    Nothing -> Left (InformError "can only have 'this' inside of class!")
                                                                    Just _  -> case M.lookup other ctxt of
                                                                      Nothing         -> case cl_name of 
                                                                                      Nothing -> Left (Unbound str) -- str
                                                                                      Just cl -> case M.lookup cl cvartable of 
                                                                                        Nothing            -> Left (Unbound str)
                                                                                        Just (flds, lvars) -> case M.lookup str flds of 
                                                                                          Nothing -> case M.lookup str lvars of 
                                                                                            Nothing -> Left (Unbound str)
                                                                                            Just t1 -> Right t1 
                                                                                          Just t  -> Right t 
                                                                      Just other_type -> case M.lookup (getTypeName other_type) cvartable of
                                                                                          Nothing -> Left (FieldError (getTypeName other_type))
                                                                                          Just (flds, l_vars) -> case M.lookup str flds of
                                                                                                   Nothing -> Left (FieldError str)
                                                                                                   Just typ -> Right typ
                                                             LExpr pat@(ObjField rexp2 str2) -> inferLExpr ctable cl_name cvartable ctxt pat
                                                             p    -> Left (InformError ("did not anticipate case for "++ show p))
