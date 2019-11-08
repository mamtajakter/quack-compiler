{
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parser.QuackParser where

import Parser.Lexer.QuackLexer
import Prelude hiding ( GT, LT, EQ)
import Control.Monad
import System.Environment ( getArgs, getProgName )
import System.Directory ( doesFileExist )
import Data.Maybe (fromJust)
import Control.Monad.IO.Class
import Parser.QuackAST
import Control.Monad.State.Lazy
}


%name parseQuack
%tokentype {Lexeme}
%error {parseError}
%monad {MonadParser}


%token
    num                   {Lexeme (LINT $$) _ _}
    str                   {Lexeme (LSTRING $$) _ _}
    ident                 {Lexeme (LID $$) _ _}
    class                 {Lexeme (LCLASS) _ _}
    def                   {Lexeme LDEF _ _}
    if                    {Lexeme LIF _ _}
    elif                  {Lexeme LELIF _ _}
    else                  {Lexeme LELSE _ _}
    while                 {Lexeme LWHILE _ _}
    return                {Lexeme LRETURN _ _}
    typecase              {Lexeme LTYPECASE _ _}
    extends               {Lexeme LEXTENDS _ _}
    true                  {Lexeme LTRUE _ _}
    false                 {Lexeme LFALSE _ _}
    or                    {Lexeme LOR _ _}
    and                   {Lexeme LAND _ _}
    not                   {Lexeme LNOT _ _}
    ">="                  {Lexeme LGE _ _}
    '>'                   {Lexeme LGT _ _}
    "=="                  {Lexeme LEQUALS _ _}
    '<'                   {Lexeme LLT _ _}
    "<="                  {Lexeme LLE _ _}
    '='                   {Lexeme LLEQ _ _}
    '/'                   {Lexeme LDIVIDE _ _}
    '*'                   {Lexeme LTIMES _ _}
    '-'                   {Lexeme LMINUS _ _}
    '+'                   {Lexeme LPLUS _ _}
    '.'                   {Lexeme LDOT _ _}
    '{'                   {Lexeme LBRACEL _ _}
    '}'                   {Lexeme LBRACER _ _}
    '('                   {Lexeme LPARENL _ _}
    ')'                   {Lexeme LPARENR _ _}
    ';'                   {Lexeme LSEMICOLON _ _}
    ':'                   {Lexeme LCOLON _ _}
    ','                   {Lexeme LCOMMA _ _}


%left or
%left and
%nonassoc not
%nonassoc '>' '<' "<=" ">=" "=="
%left '+' '-'
%left '*' '/'
%nonassoc '(' ')'
%left NEG
%left '.'


%%

Prog  : Cl Stmt                                             {Prog $1 $2}


Cl    : {- empty -}                                         {[]}
      | Cl Class                                            {$1 ++ [$2] }  -- reversed

Stmt  : {- empty -}                                         {[]}
      | Stmt Statement                                      {$1 ++ [$2]}    -- reversed

Statement : LExpr Ident1 '=' RExpr ';'                      {Assignment $1 $2 $4}
          | RExpr ';'                                       {StatementR (desugarRexpr $1)}
          | if RExpr StmtBlock RexpStmtB StmtBlock1         {If (desugarRexpr $2, $3) $4 $5}
          | while RExpr StmtBlock                           {While (desugarRexpr $2, $3)}
          | return MRExpr ';'                               {Return $2}
          | typecase RExpr '{' TyAlt '}'                    {TyCase (desugarRexpr $2) $4}

TyAlt     : {- empty -}                                     {[]}
          | TyAlt TyAltR                                    {$1 ++ [$2]}  -- reversed

TyAltR    : TyAssign StmtBlock                              {($1, $2)}

MRExpr    : {-empty-}                                       {Nothing}
          | RExpr                                           {Just $ desugarRexpr $1}

RexpStmtB  : {- empty -}                                    {[]}
           | RexpStmtB RexpStmt                             {$1 ++ [$2]}     -- reversed

RexpStmt   : elif RExpr StmtBlock                           {If (desugarRexpr $2, $3) [] Nothing}

StmtBlock1 : {- empty -}                                    {Nothing}
           | else StmtBlock                                 {Just $2}

LExpr     : ident                                           {Lid $1}
          | RExpr '.' ident                                 {ObjField (desugarRexpr $1) $3}

RExpr      : RExpr '+' RExpr                                {desugarRexpr $ Plus  $1 $3} -- Constr (LExpr $1)
           | RExpr '-' RExpr                                {desugarRexpr $ Minus $1 $3}
           | RExpr '*' RExpr                                {desugarRexpr $ Times $1 $3}
           | RExpr '/' RExpr                                {desugarRexpr $ Div   $1 $3}
           | '(' RExpr ')'                                  {desugarRexpr $2}
           | '-' RExpr %prec NEG                            {Neg (desugarRexpr $2)}
           | str                                            {Str $1}
           | num                                            {Int $ toInteger $1 }
           | true                                           {Bool True }
           | false                                          {Bool False }
           | LExpr                                          {LExpr $1}
           | RExpr "==" RExpr                               {desugarRexpr $ PEQ $1 $3}
           | RExpr "<=" RExpr                               {desugarRexpr $ PLEQ $1 $3}
           | RExpr ">=" RExpr                               {desugarRexpr $ GEQ $1 $3}
           | RExpr '>' RExpr                                {desugarRexpr $ PGT  $1 $3}
           | RExpr '<' RExpr                                {desugarRexpr $ PLT $1 $3}
           | RExpr and RExpr                                {AND $1 $3}
           | RExpr or RExpr                                 {OR $1 $3}
           | not RExpr                                      {NOT $2}
           | RExpr '.' ident '(' ActArgs ')'                {RArgs $1 $3 $5}
           | ident '(' ActArgs ')'                          {Constr $1 $3}


ActArgs  : {- empty -}                                      {Nothing}
         | RExpr RExprs1                                    {Just (desugarRexpr $1 : $2)}

RExprs1  : {- empty -}                                      {[]}
         | RExprs1 RExprs                                   {$1 ++ [$2]}   -- reversed

RExprs   : ',' RExpr                                        {desugarRexpr $2}





-- classes

Class : ClassSig ClassBody                                  {Class $1 $2}

ClassSig : class ident '(' FormArgs ')' Exten             {% state (\s -> (ClassSig $2 $4 $6, (PState $2)))}

Exten : {- empty -}                                         {Just "Obj"}
      | extends ident                                     {Just $2}

FormArgs : {- empty -}                                      {[]}
         | ident  ':' ident ArgList1                        {(TyAssign ($1, $3):$4)}


ArgList1 : {-empty -}                                       {[]}
        | ArgList1 ',' TyAssign                              {$1 ++ [$3]}              -- reversed

TyAssign  : ident ':' ident                                  {TyAssign ($1, $3)}

ClassBody : '{' Stmt Method1 '}'                            {ClassBody $2 $3}

Method1   : {- empty -}                                     {[]}
          | Method1 Method                                  {$1 ++ [$2]}              -- reversed

Method    : def ident '(' FormArgs ')' Ident11  StmtBlock    {% get >>= \(PState s) -> return $ Method s $2 $4 $6 $7}

Ident11   : {- empty -}                                     {Just "Nothing"}
          | ':' ident                                       {Just $2}

Ident1    : {- empty -}                                     {Nothing}
          | ':' ident                                       {Just $2}

StmtBlock  : '{' Stmt '}'                                   {$2}



{

getLexemeClass :: Lexeme -> LexemeClass
getLexemeClass (Lexeme cls _ _) = cls

parseError :: [Lexeme] -> MonadParser a
parseError l   = failE ("Parse Error on token (" ++ show (head l) ++ ")")

getString :: LexemeClass -> Maybe String
getString (LSTRING str) = Just str
getString _             = Nothing -- error "getString called on non-string token"

getInt :: LexemeClass -> Maybe Integer
getInt (LINT int) = Just $ toInteger int
getInt _          = Nothing -- error "getInt called on non-integer token"



data MonadParser a = MParser {runParser :: ParserState -> Either String (a, ParserState)}


data ParserState = PState
                 {
                   className :: String
                 }

startState :: ParserState
startState = PState ""

instance Functor MonadParser where
  fmap f mp  = MParser $ \ps ->
               case runParser mp ps of
                     Left msg       -> Left msg
                     Right (a, pst) -> Right (f a, pst)


instance Applicative MonadParser where
  pure       = return
  (<*>)        = ap


instance Monad MonadParser where
  return x = MParser $ \ps -> Right (x, ps)
  m >>= k  = MParser $ \ps -> case runParser m ps of
                        Left msg       -> Left msg
                        Right (a, pst) -> runParser (k a) pst


instance MonadState ParserState MonadParser where
  get        = MParser $ \s -> Right(s,s)
  put ps     = MParser $ \_ -> Right ((), ps)
  state func = MParser $ \s -> Right $ func s



failE :: String -> MonadParser a
failE err = MParser $ \_ -> Left err


mainParse :: String -> IO (MonadParser Prog)
mainParse filename =
  do
     flag <- doesFileExist filename
     when (not flag) (error' ("The following file does not exist : " ++ filename))
     putStrLn ("Parsing Quack program in file " ++ filename)
     s <- readFile filename
     let sr = scanner s
     case sr of
         Left msg      -> return $ failE msg
         Right tokList -> return $ parseQuack $ init tokList




}
