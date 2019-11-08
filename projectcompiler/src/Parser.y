{

{-# LANGUAGE GADTs #-}
module Parser where

import Lexer
import System.Directory ( doesFileExist )

import Control.Monad



import Prelude hiding ( GT, LT, EQ)
import System.Environment ( getArgs, getProgName )
import System.Directory ( doesFileExist )
import Data.Maybe (fromJust)
}



%name parserCalc
%tokentype {LexemeClass}
%error {parseError}

%token
  eof             { EOF } 
  ident           { LID $$ } 
  integer_literal { LINT $$ } 
  string_literal  { LSTRINGLITERAL $$ }
  class           { LCLASS }
  def             { LDEF } 
  extends         { LEXTENDS }
  if              { LIF }
  elif            { LELIF }
  else            { LELSE }
  while           { LWHILE }
  return          { LRETURN }
  typecase        { LTYPECASE }
  and             { LAND }
  or              { LOR }
  not             { LNOT }
  '='             { LGETS }
  '+'             { LPLUS }
  '-'             { LMINUS }
  '*'             { LTIMES }
  '/'             { LDIVIDE }
  '=='            { LEQUALS }
  '>='            { LATMOST }
  '<'             { LLESS }
  '<='            { LATLEAST }
  '>'             { LMORE }
  '{'             { LLBRACE }
  '}'             { LRBRACE }
  '('             { LLPAREN }
  ')'             { LRPAREN }
  ','             { LCOMMA }
  ';'             { LSEMICOLON }
  '.'             { LDOT }
  ':'             { LCOLON }

%left or
%left and  
%nonassoc not
%nonassoc '>' '<' '<=' '>=' '=='
%left '+' '-'
%left '*' '/'
%left '.'
%left NEG

%%

Prog : Cl Stmts eof                                            {Prog $1 $2}

Cl : {-empty-}                                                 {[]}
   | Cl ClassSigBody                                           {$2 : $1}

ClassSigBody : ClassSignature ClassBody                        {ClassSigBody $1 $2}

ClassSignature : class ident '(' FormalArgs ')' Exten      {ClassSignature $2 $4 $6}

Exten : {- empty -}                                         {Nothing}
      | extends ident                                       {Just $2}


FormalArgs : {-empty-}                                         {FormalArgs Nothing}
           | ident ':' ident IDs                               {FormalArgs (Just (IDs ($1, $3):$4)) }

IDs : {-empty-}                                                {[]}
    | IDs ',' ident ':' ident                                  {IDs ($3, $5): $1}

ClassBody : '{' Stmts Methods '}'                               {ClassBody $2 $3}


Methods   : {- empty -}                                     {[]}
          | Methods Method                                  {$2 : $1}              -- reversed

Method    : def ident '(' FormalArgs ')' Ident1  StmtBlock    {Method $2 $4 $6 $7}

Ident1    : {- empty -}                                     {Nothing}
          | ':' ident                                       {Just $2}




StmtBlock : '{' Stmts   '}'                                    { $2 }

Stmts : {-empty-}                                              {[]}
     | Stmts Stmt                                              {$2 : $1}

Stmt : if RExpr StmtBlock ElifStmt ElseStmt                    {If ($2,$3) $4 $5 }
     | while RExpr StmtBlock                                   {While ($2,$3)}
     | LExpr SIdent '=' RExpr ';'                              {Stmt $1 $2 $4}
     | RExpr ';'                                               {StmtR $1}
     | return SRExp';'                                         {Return $2}
     | typecase RExpr '{' TypeAlternative '}'                  {TyCase $2 $4}

SRExp : {-empty-}                                              {Nothing}
      | RExpr                                                  {Just $1}

SIdent : {-empty-}                                             {Nothing}
       | ':' ident                                             {Just $2}

ElifStmt : {-empty-}                                           {[]}
         | ElifStmt elif RExpr StmtBlock                       {($3,$4): $1}

ElseStmt : {-empty-}                                           {Nothing}
         | else StmtBlock                                      {Just $2}

TypeAlternative : {-empty-}                                    {[]}
                | TypeAlternative TyAltR                       {$2:$1 }



TyAltR    : ident ':' ident StmtBlock                       {TypeAlternative ($1, $3) $4}


LExpr : ident                                                  {LId $1}
      | RExpr '.' ident                                        {LObjId $1 $3}

RExpr : string_literal                                         {Str $1}
      | integer_literal                                        {Int ( toInteger $1)}
      | LExpr                                                  {LExpr $1}
      | RExpr '+' RExpr                                        {Plus $1 $3}
      | RExpr '-' RExpr                                        {Minus $1 $3}
      | RExpr '*' RExpr                                        {Times $1 $3}
      | RExpr '/' RExpr                                        {Divide $1 $3}
      | '-' RExpr %prec NEG                                    {Neg $2}
      | '(' RExpr ')'                                          {Paren $2}
      | RExpr '==' RExpr                                       {Equals $1 $3}
      | RExpr '<=' RExpr                                       {AtMost $1 $3}
      | RExpr '<' RExpr                                        {Less $1 $3}
      | RExpr '>=' RExpr                                       {AtLeast $1 $3}
      | RExpr '>' RExpr                                        {More $1 $3}
      | RExpr and RExpr                                        {AND $1 $3}
      | RExpr or RExpr                                         {OR $1 $3}
      | not RExpr                                              {NOT $2}
      | RExpr '.' ident '(' ActualArgs ')'                     {RArgs $1 $3 $5}
      | ident '(' ActualArgs ')'                               {Constr $1 $3}

ActualArgs :  {-empty-}                                        { Nothing}
           | RExpr ActArg                                      { Just ($1,$2)}

ActArg : {-empty-}                                             {[]}
       | ActArg ',' RExpr                                      {$3 : $1}


{

data Prog where
  Prog :: [ClassSigBody] -> [Stmt] -> Prog
  deriving (Show)

data ClassSigBody where
  ClassSigBody :: ClassSignature -> ClassBody -> ClassSigBody
  deriving (Show)

data ClassSignature where
  ClassSignature :: String -> FormalArgs -> Maybe String -> ClassSignature
  deriving (Show)

data FormalArgs where
  FormalArgs :: Maybe [IDs] -> FormalArgs
  deriving (Show)

data IDs = IDs (String, String) deriving (Show)

data Stmt where
  Stmt  :: LExpr -> (Maybe String) -> RExpr -> Stmt
  StmtR :: RExpr -> Stmt
  If         :: (RExpr, StmtBlock)
             -> [(RExpr, StmtBlock)]
             -> Maybe StmtBlock         -> Stmt
  While      :: (RExpr, StmtBlock)      -> Stmt
  Return     :: Maybe RExpr             -> Stmt
  TyCase     :: RExpr                   -> [TypeAlternative] -> Stmt
  deriving Show

data TypeAlternative where 
   TypeAlternative :: (String,String)-> StmtBlock -> TypeAlternative
   deriving Show

data RExpr where
  Str    :: String  -> RExpr
  Int    :: Integer -> RExpr
  LExpr  :: LExpr   -> RExpr
  Plus   :: RExpr   -> RExpr   -> RExpr
  Minus  :: RExpr   -> RExpr   -> RExpr
  Times  :: RExpr   -> RExpr   -> RExpr
  Divide    :: RExpr   -> RExpr   -> RExpr
  Neg    :: RExpr   -> RExpr
  Paren  :: RExpr   -> RExpr
  Equals     :: RExpr   -> RExpr   -> RExpr
  AtMost    :: RExpr   -> RExpr   -> RExpr
  Less    :: RExpr   -> RExpr   -> RExpr
  AtLeast     :: RExpr   -> RExpr   -> RExpr
  More     :: RExpr   -> RExpr   -> RExpr
  AND    :: RExpr   -> RExpr   -> RExpr
  OR     :: RExpr   -> RExpr   -> RExpr
  NOT    :: RExpr   -> RExpr
  RArgs  :: RExpr   -> String  -> ActualArgs -> RExpr
  Constr :: String  -> ActualArgs -> RExpr
  deriving Show

{-
data ActualArgs where
  ActualArgs :: Maybe  (RExpr, [RExpr]) -> ActualArgs
  deriving (Show)
-}

type ActualArgs = Maybe (RExpr, [RExpr])

data LExpr where
  LId     :: String -> LExpr
  LObjId :: RExpr  -> String -> LExpr
  deriving Show

data ClassBody where
  ClassBody :: [Stmt] -> [Method] -> ClassBody
  deriving (Show)

data Method where
   Method :: String -> FormalArgs -> Maybe String -> StmtBlock -> Method
   deriving Show

type StmtBlock = [Stmt]

parseError :: [LexemeClass] -> a
parseError x  = error ("Parse error\n" ++ " Invalid Syntax at the First token of " ++ show (take 10 x) ++ "..." )

fixer:: Lexeme ->  LexemeClass
fixer (Lexeme _ x _) = x

main_Parser :: String -> IO ()
main_Parser filename=
    do flag <- doesFileExist filename
       when (not flag) (error ("The following file does not exist : " ++ filename))
       putStrLn ("Beginning Parsing of the Quack program in file " ++  filename)
       s <- readFile filename
       let sr = scanner s
       case sr of
            Left st  -> error st
            Right ls -> print (parserCalc (map fixer ls))
  

}

