{-# LANGUAGE GADTs #-}

module TypeChecker.QuackTypes where

-- import Parser.QuackAST
import qualified Data.Map as M
import Data.Maybe


data QuackType where
  -- bottom is the lowest class in the hierarchy
  Bottom  :: QuackType
  -- Every class extends obj. Obj itself is a subtype of
  Obj     :: QuackType
  Boolean :: QuackType
  String  :: QuackType
  TNothing :: QuackType
  Int     :: QuackType
  UserDfn :: Name      -> QuackType   -> QuackType
  deriving Show

  -- the signature of a user defined type is its name, its constructor's signature
  -- and the class it extends, if not present, class is Obj.
-- type CSig        = (ConstrSig, QuackType)
type Name        = String
-- type ConstrSig   = [QuackType]  -- class constructor signature
type TyCtx       = M.Map String QuackType     -- the typing context
-- the class table is a mapping from class names to a list of their method signatures.
type CTable      = M.Map Name MSigTable
-- a method signature is the signature of its class and the signature of its formal arguments, and return arguments
-- signature of its class can be defined as just the class it extends
type MSigTable   = M.Map Name (QuackType, [QuackType])

type CVarTable    = M.Map Name (TyCtx, TyCtx)        -- class name maps to (fields, local_variables)
type MthdTable    = M.Map (Name, (QuackType, [QuackType])) Name -- to help with code gen 
type MethodT      = M.Map (Name, Name) (QuackType, [QuackType]) -- to help with code gen

type LocalMVars   = M.Map Name TyCtx           -- method name together with all local variables of the method!
type MethodVars   = M.Map Name  LocalMVars     -- class name with all methods and their local variables!


initCTable :: CTable
initCTable = M.fromList [
            (
            "Boolean" , M.fromList [
                         ("EQUALS", (Obj, [Obj, Obj, Boolean]))
                       , ("STR", (Obj , [Boolean, String]))
                       , ("PRINT"  , (Obj, [Obj, Obj]))
                       , ("constructor", (Obj, [Boolean]))
                       ]
            ),
            (
             "Int" ,    M.fromList [
                          ("PLUS", (Obj, [Obj, Obj, Int]))
                        , ("constructor", (Obj,  [Int]))
                        , ("STR"  , (Obj, [Int, String]))
                        , ("PRINT"  , (Obj, [Obj, Obj]))
                        , ("EQUALS"  , (Obj, [Int, Obj, Boolean]))
                        , ("LESS"  ,  (Obj, [Int, Int, Boolean]))

                       ]
            ),
            (
             "String" , M.fromList [
                        ("constructor",  (Obj, [String]))
                      , ("PRINT",  (Obj, [String, String]))
                      , ("STR",  (Obj, [String, String]))
                      , ("EQUALS",  (Obj, [String, Obj, Boolean]))
                      , ("LESS",  (Obj, [String, String, Boolean]))
                      , ("PLUS", (Obj, [Obj, Obj, String]))
                       ]
            ),
            (
            "Obj" , M.fromList [
                      ("constructor",  (Obj, [Obj]))
                    , ("STR",  (Obj, [Obj, String]))
                    , ("PRINT",  (Obj, [Obj, Obj]))
                    , ("EQUALS", (Obj, [Obj, Obj, Boolean]))
                   ]
            ),
            (
            "Nothing", M.fromList [
                        ("constructor", (Obj, [TNothing]))

                      ]
            )

           ]

genMethodTable :: CTable -> MethodT 
genMethodTable = genMethT.genMethTable'

genMethT  :: MthdTable -> MethodT
genMethT mt = M.fromList $ map (\((mname, sig), clname) -> ((mname, clname), sig)) (M.toList mt)

genMethTable' :: CTable -> MthdTable
genMethTable' ct = M.fromList (concat (map (\(cname, tb) -> 
                     map (\methd -> (methd, cname)) (M.toList tb)) (M.toList ct)))

findMethodOfClass :: String -> (QuackType, [QuackType]) -> QuackType -> M.Map String MSigTable -> Maybe (QuackType, [QuackType])
findMethodOfClass method_name meth_sig qtype ctable = case qtype of
          Bottom -> Nothing
          Obj    -> if method_name `elem` ["constructor", "STR", "PRINT", "EQUALS"]
                       then
                         let meth_list = fromJust $ M.lookup "Obj" ctable in
                           let (t, t_list) = fromJust $ M.lookup method_name meth_list in
                              if isMethodSubTypeOf meth_sig (t, init t_list) then Just (t, t_list) else Nothing
                       else Nothing

          UserDfn name exten -> let meth_list = fromJust $ M.lookup name ctable in
                                  let loop = findMethodOfClass method_name meth_sig exten ctable in
                                    case M.lookup method_name meth_list of
                                           Nothing -> loop
                                           Just (t, t_list) -> if isMethodSubTypeOf meth_sig (t, t_list) -- init t_list
                                                           then Just (t, t_list)
                                                              else loop

          Int         -> findMethodOfClass method_name meth_sig (UserDfn "Int" Obj) ctable
          Boolean     -> findMethodOfClass method_name meth_sig (UserDfn "Boolean" Obj) ctable
          String      -> findMethodOfClass method_name meth_sig (UserDfn "String" Obj) ctable
          TNothing      -> findMethodOfClass method_name meth_sig (UserDfn "Nothing" Obj) ctable

isListSubTypeOf :: [QuackType] -> [QuackType] -> Bool
isListSubTypeOf [] []         = True
isListSubTypeOf [] (x:xs)     = False
isListSubTypeOf (x:xs) []     = False
isListSubTypeOf (x:xs) (y:ys) = isSubTypeOf x y && isListSubTypeOf xs ys

isMethodSubTypeOf :: (QuackType, [QuackType]) -> (QuackType, [QuackType]) -> Bool
isMethodSubTypeOf (t1, []) (t2, [])       = True
isMethodSubTypeOf (t1, []) (t2, (x:xs))   = True
isMethodSubTypeOf (t1, (x:xs)) (t2, [])   = False
isMethodSubTypeOf (t1, args1) (t2, args2) = isListSubTypeOf (init args1) (init args2) && isSubTypeOf (last args1) (last args2)

isSubTypeOf :: QuackType -> QuackType -> Bool
isSubTypeOf Bottom _        = True
isSubTypeOf Obj Obj         = True
isSubTypeOf Obj _           = False
isSubTypeOf Boolean Boolean = True
isSubTypeOf Boolean Obj     = True
isSubTypeOf Boolean _       = False
isSubTypeOf Int Int         = True
isSubTypeOf Int Obj         = True
isSubTypeOf Int _           = False
isSubTypeOf String String   = True
isSubTypeOf String Obj      = True
isSubTypeOf String _        = False
isSubTypeOf (UserDfn name extension) ext@(UserDfn name1 extension1) = extension `isSubTypeOf` ext || extension `isSubTypeOf` extension1
isSubTypeOf (UserDfn name extension) ty                         = isSubTypeOf extension ty
isSubTypeOf TNothing TNothing = True
isSubTypeOf Bottom TNothing  = True
isSubTypeOf _       TNothing = False
isSubTypeOf TNothing Obj     = True
isSubTypeOf TNothing _       = False

getTypeName :: QuackType -> String
getTypeName (UserDfn name ty) = name
getTypeName ty                = show ty

instance Eq QuackType where
  (==) ty1 ty2 = isSubTypeOf ty1 ty2 && isSubTypeOf ty2 ty1

instance Ord QuackType where
  --compare :: a -> a -> Ordering
  --(<) :: a -> a -> Bool
  (<) = isSubTypeOf
  --(<=) :: a -> a -> Bool
  (<=) a b = isSubTypeOf a b || (a == b)
  --(>) :: a -> a -> Bool
  --(>) = not.(<=)
  --(>=) :: a -> a -> Bool
  --max :: a -> a -> a
  max a b = if isSubTypeOf a b then b else a 
  --min :: a -> a -> a

makeQuackTypeFromStr :: String -> CTable -> Maybe QuackType
makeQuackTypeFromStr "Obj" _       = Just Obj 
makeQuackTypeFromStr "String" _    = Just String  
makeQuackTypeFromStr "Int" _       = Just Int 
makeQuackTypeFromStr "Nothing" _   = Just TNothing
makeQuackTypeFromStr "Bottom" _    = Just Bottom 
makeQuackTypeFromStr name   ctable = case M.lookup name ctable of 
                             Nothing      -> Nothing 
                             Just mtable -> case M.lookup "constructor" mtable of 
                              Nothing     -> Nothing 
                              Just (t, _) -> Just (UserDfn name t) 