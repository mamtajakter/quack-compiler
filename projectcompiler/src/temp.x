{
module Parser where

import Lexer
import System.Directory ( doesFileExist )

import Control.Monad
}

%name parserCalc
%tokentype {LexemeClass}
%monad { P } { thenP } { returnP }

%token
  eof             { EOF } 
  ident           { ID $$ } 
  integer_literal { INT $$ } 
  string_literal  { STRINGLITERAL $$ }
  class           { CLASS }
  def             { DEF } 
  extends         { EXTENDS }
  if              { IF }
  elif            { ELIF }
  else            { ELSE }
  while           { WHILE }
  return          { RETURN }
  typecase        { TYPECASE }
  String          { STRING }
  Integer         { INTEGER }
  Obj             { OBJ }
  Boolean         { BOOLEAN }
  true            { TRUE }
  false           { FALSE }
  and             { AND }
  or              { OR }
  not             { NOT }
  Nothing         { NOTHING }
  none            { NONE }
  '='             { GETS }
  '+'             { PLUS }
  '-'             { MINUS }
  '*'             { TIMES }
  '/'             { DIVIDE }
  '=='            { EQUALS }
  '>='            { ATMOST }
  '<'             { LESS }
  '<='            { ATLEAST }
  '>'             { MORE }
  '{'             { LBRACE }
  '}'             { RBRACE }
  '('             { LPAREN }
  ')'             { RPAREN }
  ','             { COMMA }
  ';'             { SEMICOLON }
  '.'             { DOT }
  ':'             { COLON }

%left or
%left and  
%nonassoc not
%nonassoc '>' '<' '<=' '>=' '=='
%left '+' '-'
%left '*' '/'
%left '.'
%left NEG

%%



Prog : Cl Stmts eof                                            {}

Cl : {-empty-}                                                 {}
   | Cl ClassSigBody                                           {}

ClassSigBody : ClassSignature ClassBody                        {}

ClassSignature : CS                                            {}
               | CS extends TypeName                              {}

CS : class ident '(' FormalArgs ')'                            {}

TypeName : Obj  {}
         |Boolean  {}
         | Integer {}
         | String {}
         | ident  {}



FormalArgs : {-empty-}                                         {}
           | ident ':' TypeName IDs                               {}

IDs : {-empty-}                                                {}
    | IDs ',' ident ':' TypeName                                  {}

ClassBody : '{' Stmts Method '}'                               {}

Method : {-empty-}                                             {}
       | Method MethodRest                                     {}


MethodRest : MR StmtBlock                                      {}
       | MR ':' ident  StmtBlock                               {}


MR : def ident '(' FormalArgs ')'                              {}

StmtBlock : '{' Stmts   '}'                                    {}

Stmts : {-empty-}                                              {}
     | Stmts Stmt                                              {}


Stmt : IfStmt EStmt                                            {}
     | while RExpr StmtBlock                                   {}
     | LExpr AStmt                                             {}
     | LExpr ':' ident AStmt                                   {}
     | RExpr ';'                                               {}
     | return ';'                                              {}
     | return RExpr ';'                                        {}
     | Typecase                                                {}
                                              
     

IfStmt : if RExpr StmtBlock                                    {}

EStmt : ElifStmt  ElseStmt                                     {} 

ElifStmt : {-empty-}                                           {}
         | ElifStmt elif RExpr StmtBlock                       {}

ElseStmt : {-empty-}                                           {}
         | else StmtBlock                                      {}


AStmt : '=' RExpr ';'                                          {}



Typecase : typecase RExpr '{' TypeAlternative '}'              {}

TypeAlternative : {-empty-}                                    {}
                | TypeAlternative ident ':' ident StmtBlock    {}


LExpr : ident                                                  {}
      | RExpr '.' ident                                        {}

RExpr : string_literal                                         {}
      | integer_literal                                        {}
      | LExpr                                                  {}
      | RExpr '+' RExpr                                        {}
      | RExpr '-' RExpr                                        {}
      | RExpr '*' RExpr                                        {}
      | RExpr '/' RExpr                                        {}
      | '-' RExpr %prec NEG                                    {}
      | '(' RExpr ')'                                          {}
      | RExpr '==' RExpr                                       {}
      | RExpr '<=' RExpr                                       {}
      | RExpr '<' RExpr                                        {}
      | RExpr '>=' RExpr                                       {}
      | RExpr '>' RExpr                                        {}
      | RExpr and RExpr                                        {}
      | RExpr or RExpr                                         {}
      | not RExpr                                              {}
      | RExpr '.' ident '(' ActualArgs ')'                     {}
      | ident '(' ActualArgs ')'                               {}

ActualArgs :  {-empty-}                                        {}
           | RExpr ActArg                                      {}

ActArg : {-empty-}                                             {}
       | ActArg ',' RExpr                                      {}


{

data ParseResult a = ParseOk a | ParseFail String

type P a = Int -> ParseResult a 

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \l -> case m l of
                         ParseFail s -> ParseFail s
                         ParseOk a -> k a l

returnP :: a -> P a
returnP a = \l -> ParseOk a

parseError :: [LexemeClass] -> a
parseError x  = error ("Parse error\n" ++ " Invalid Syntax at the First token of " ++ show (take 10 x) )

fixer:: Lexeme ->  LexemeClass
fixer (Lexeme _ x _) = x

happyError = \tks i -> error ( "Parse error in line " ++ show (i::Int) ++ "\n")

runCalc :: String -> Exp
runCalc s = case calc (lexer s) 1 of
                      ParseOk e -> e
                      ParseFail s -> error s

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






=============================
RExpr : RExpr '+' RExpr {}
RExpr : Basic {}
RExpr : RExpr '==' RExpr {}


Basic : '-' Basic {}
Basic : Lexpr {}
Lexpr : ident {}




-----------------

LExpr : ident                                           {}
LExpr : RExpr '.' ident                                 {}

RExpr : string_literal                                  {}
RExpr : integer_literal                                 {}
RExpr : LExpr                                           {}
RExpr : RExpr '+' RExpr                                 {}
RExpr : RExpr '-' RExpr                                 {}
RExpr : RExpr '*' RExpr                                 {}
RExpr : RExpr '/' RExpr                                 {}
RExpr : '-' RExpr %prec NEG                             {}
RExpr : '(' RExpr ')'                                   {}
RExpr : RExpr '==' RExpr                                {}
RExpr : RExpr '<=' RExpr                                {}
RExpr : RExpr '<' RExpr                                 {}
RExpr : RExpr '>=' RExpr                                {}
RExpr : RExpr '>' RExpr                                 {}
RExpr : RExpr and RExpr                                 {}
RExpr : not RExpr                                       {}
RExpr : RExpr '.' ident '(' ActualArgs ')'              {}
RExpr : ident '(' ActualArgs ')'                        {}

ActualArgs :  {-empty-}                                 {}
           | RExpr ActArg                               {}

ActArg : {-empty-}                                      {}
       | ActArg ',' RExpr                               {}


=================================================================

Prog : Cl Stmts                                                {}

Cl : {-empty-}                                                 {}
   | Cl ClassSigBody                                           {}

ClassSigBody : ClassSignature ClassBody                        {}

ClassSignature : CS                                            {}
               | CS extends ident                              {}

CS : class ident '(' FormalArgs ')'                            {}

FormalArgs : {-empty-}                                         {}
           | ident ':' ident IDs                               {}

IDs : {-empty-}                                                {}
    | IDs ',' ident ':' ident                                  {}

ClassBody : '{' Stmts Method '}'                               {}

Method : {-empty-}                                             {}
       | Method MethodRest                                     {}


MethodRest : MR StmtBlock                                      {}
       | MR ':' ident  StmtBlock                               {}


MR : def ident '(' FormalArgs ')'                              {}

StmtBlock : '{' Stmts   '}'                                    {}

Stmts : {-empty-}                                              {}
     | Stmts Stmt                                              {}


Stmt : IfStmt EStmt                                            {}
     | while RExpr StmtBlock                                   {}
     | LExpr AStmt                                             {}
     | LExpr ':' ident AStmt                                   {}
     | RExpr ';'                                               {}
     | return ';'                                              {}
     | return RExpr ';'                                        {}
     | Typecase                                                {}
                                              
     

IfStmt : if RExpr StmtBlock                                    {}

EStmt : ElifStmt  ElseStmt                                     {} 

ElifStmt : {-empty-}                                           {}
         | ElifStmt elif RExpr StmtBlock                       {}

ElseStmt : {-empty-}                                           {}
         | else StmtBlock                                      {}


AStmt : '=' RExpr ';'                                          {}



Typecase : typecase RExpr '{' TypeAlternative '}'              {}

TypeAlternative : {-empty-}                                    {}
                | TypeAlternative ident ':' ident StmtBlock    {}


LExpr : ident                                           {}
      | RExpr '.' ident                                 {}

RExpr : string_literal                                  {}
      | integer_literal                                 {}
      | LExpr                                           {}
      | RExpr '+' RExpr                                 {}
      | RExpr '-' RExpr                                 {}
      | RExpr '*' RExpr                                 {}
      | RExpr '/' RExpr                                 {}
      | '-' RExpr %prec NEG                             {}
      | '(' RExpr ')'                                   {}
      | RExpr '==' RExpr                                {}
      | RExpr '<=' RExpr                                {}
      | RExpr '<' RExpr                                 {}
      | RExpr '>=' RExpr                                {}
      | RExpr '>' RExpr                                 {}
      | RExpr and RExpr                                 {}
      | RExpr or RExpr                                  {}
      | not RExpr                                       {}
      | RExpr '.' ident '(' ActualArgs ')'              {}
      | ident '(' ActualArgs ')'                        {}

ActualArgs :  {-empty-}                                 {}
           | RExpr ActArg                               {}

ActArg : {-empty-}                                      {}
       | ActArg ',' RExpr                               {}





===========================================================

prec :: { Int }
      : int {% if $1 < 0 || $1 > 9
               then failE "Precedence out of range"
           else returnE $1
} 

lexer :: String -> [LexemeClass]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexIdentifier (c:cs)
      | isDigit c = lexNumber (c:cs)
lexer ('=':cs) = GETS : lexer cs
lexer ('+':cs) = PLUS : lexer cs
lexer ('-':cs) = MINUS : lexer cs
lexer ('*':cs) = TIMES : lexer cs
lexer ('/':cs) = DIVIDE : lexer cs
lexer ('=':'=':cs) = EQUALS : lexer cs
lexer ('<':'=':cs) = ATMOST : lexer cs
lexer ('>':cs) = MORE : lexer cs
lexer ('>':'=':cs) = ATLEAST : lexer cs
lexer ('<':cs) = LESS : lexer cs
lexer ('{':cs) = LBRACE : lexer cs
lexer ('}':cs) = RBRACE : lexer cs
lexer ('(':cs) = LPAREN : lexer cs
lexer (')':cs) = RPAREN : lexer cs
lexer (',':cs) = COMMA : lexer cs
lexer (';':cs) = SEMICOLON : lexer cs
lexer ('.':cs) = DOT : lexer cs
lexer (':':cs) = COLON : lexer cs
lexNum cs = TokenInt (read num) : lexer rest
       where (num,rest) = span isDigit cs
lexVar cs = case span isAlpha cs of
       ("class",rest) -> CLASS : lexer rest
       ("def",rest) -> DEF : lexer rest
       ("extends",rest) -> EXTENDS : lexer rest
       ("if",rest) -> IF : lexer rest
       ("elif",rest) -> ELIF : lexer rest
       ("else",rest) -> ELSE : lexer rest
       ("while",rest) -> WHILE : lexer rest
       ("return",rest) -> RETURN : lexer rest
       ("typecase",rest) -> TYPECASE : lexer rest
       ("String",rest) -> STRING : lexer rest
       ("Integer",rest) -> INTEGER : lexer rest
       ("Obj",rest) -> OBJ : lexer rest
       ("Boolean",rest) -> BOOLEAN  : lexer rest
       ("true",rest) -> TRUE : lexer rest
       ("false",rest) -> FALSE : lexer rest
       ("and",rest) -> AND : lexer rest
       ("or",rest) -> OR : lexer rest
       ("not",rest) -> NOT : lexer rest 
       ("Nothing",rest) -> NOTHING : lexer rest
       ("none",rest) -> NONE : lexer rest
       (var,rest) -> TokenVar var : lexer rest


data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = case m of
                   Ok a     -> k a
                   Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = case m of
                  Ok a -> OK a
                  Failed e -> k e





           library
  hs-source-dirs:
      src
  build-depends:
      base >=4.7 && <5
  default-language: Haskell2010



  executable lexer-exe
  main-is: Lexer.hs
  other-modules:
      Parser
  hs-source-dirs:
      src
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >=4.7 && <5
    , directory >= 1.3.1.5
    , containers >= 0.5.11
    , array >= 0.5.2
  default-language: Haskell2010

executable parser-exe
  main-is: Parser.hs
  other-modules:
      Lexer
  hs-source-dirs:
      src
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >=4.7 && <5
    , directory >= 1.3.1.5
    , containers >= 0.5.11
    , array >= 0.5.2
  default-language: Haskell2010
=========================================================

{
module Parser where

import Lexer
import System.Directory ( doesFileExist )

import Control.Monad
}

%name parserCalc
%tokentype {LexemeClass}
%error {parseError}

%token
  eof             { EOF } 
  ident           { ID $$ } 
  integer_literal { INT $$ } 
  string_literal  { STRINGLITERAL $$ }
  class           { CLASS }
  def             { DEF } 
  extends         { EXTENDS }
  if              { IF }
  elif            { ELIF }
  else            { ELSE }
  while           { WHILE }
  return          { RETURN }
  typecase        { TYPECASE }
  String          { STRING }
  Integer         { INTEGER }
  Obj             { OBJ }
  Boolean         { BOOLEAN }
  true            { TRUE }
  false           { FALSE }
  and             { AND }
  or              { OR }
  not             { NOT }
  Nothing         { NOTHING }
  none            { NONE }
  '='             { GETS }
  '+'             { PLUS }
  '-'             { MINUS }
  '*'             { TIMES }
  '/'             { DIVIDE }
  '=='            { EQUALS }
  '>='            { ATMOST }
  '<'             { LESS }
  '<='            { ATLEAST }
  '>'             { MORE }
  '{'             { LBRACE }
  '}'             { RBRACE }
  '('             { LPAREN }
  ')'             { RPAREN }
  ','             { COMMA }
  ';'             { SEMICOLON }
  '.'             { DOT }
  ':'             { COLON }

%left or
%left and  
%nonassoc not
%nonassoc '>' '<' '<=' '>=' '=='
%left '+' '-'
%left '*' '/'
%left '.'
%left NEG

%%



Prog : Cl Stmts eof                                            {}

Cl : {-empty-}                                                 {}
   | Cl ClassSigBody                                           {}

ClassSigBody : ClassSignature ClassBody                        {}

ClassSignature : CS                                            {}
               | CS extends TypeName                              {}

CS : class ident '(' FormalArgs ')'                            {}

TypeName : Obj  {}
         |Boolean  {}
         | Integer {}
         | String {}
         | ident  {}



FormalArgs : {-empty-}                                         {}
           | ident ':' TypeName IDs                               {}

IDs : {-empty-}                                                {}
    | IDs ',' ident ':' TypeName                                  {}

ClassBody : '{' Stmts Method '}'                               {}

Method : {-empty-}                                             {}
       | Method MethodRest                                     {}


MethodRest : MR StmtBlock                                      {}
       | MR ':' ident  StmtBlock                               {}


MR : def ident '(' FormalArgs ')'                              {}

StmtBlock : '{' Stmts   '}'                                    {}

Stmts : {-empty-}                                              {}
     | Stmts Stmt                                              {}


Stmt : IfStmt EStmt                                            {}
     | while RExpr StmtBlock                                   {}
     | LExpr AStmt                                             {}
     | LExpr ':' ident AStmt                                   {}
     | RExpr ';'                                               {}
     | return ';'                                              {}
     | return RExpr ';'                                        {}
     | Typecase                                                {}
                                              
     

IfStmt : if RExpr StmtBlock                                    {}

EStmt : ElifStmt  ElseStmt                                     {} 

ElifStmt : {-empty-}                                           {}
         | ElifStmt elif RExpr StmtBlock                       {}

ElseStmt : {-empty-}                                           {}
         | else StmtBlock                                      {}


AStmt : '=' RExpr ';'                                          {}



Typecase : typecase RExpr '{' TypeAlternative '}'              {}

TypeAlternative : {-empty-}                                    {}
                | TypeAlternative ident ':' ident StmtBlock    {}


LExpr : ident                                                  {}
      | RExpr '.' ident                                        {}

RExpr : string_literal                                         {}
      | integer_literal                                        {}
      | LExpr                                                  {}
      | RExpr '+' RExpr                                        {}
      | RExpr '-' RExpr                                        {}
      | RExpr '*' RExpr                                        {}
      | RExpr '/' RExpr                                        {}
      | '-' RExpr %prec NEG                                    {}
      | '(' RExpr ')'                                          {}
      | RExpr '==' RExpr                                       {}
      | RExpr '<=' RExpr                                       {}
      | RExpr '<' RExpr                                        {}
      | RExpr '>=' RExpr                                       {}
      | RExpr '>' RExpr                                        {}
      | RExpr and RExpr                                        {}
      | RExpr or RExpr                                         {}
      | not RExpr                                              {}
      | RExpr '.' ident '(' ActualArgs ')'                     {}
      | ident '(' ActualArgs ')'                               {}

ActualArgs :  {-empty-}                                        {}
           | RExpr ActArg                                      {}

ActArg : {-empty-}                                             {}
       | ActArg ',' RExpr                                      {}


{



parseError :: [LexemeClass] -> a
parseError x  = error ("Parse error\n" ++ " Invalid Syntax at the First token of " ++ show (take 10 x) )

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




