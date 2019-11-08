{
{-# OPTIONS -w -funbox-strict-fields #-}
module Lexer where

import Prelude hiding ( GT, LT, EQ )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), getOpt, usageInfo )
import System.Environment ( getArgs, getProgName )
import System.Directory ( doesFileExist )
import Control.Monad
import Data.Maybe
import Numeric ( readDec )
import Data.Char ( chr )
import Data.Map ( Map )
import qualified Data.Map as Map ( empty )
}


%wrapper "monadUserState"
$digit = 0-9
$whitespace = [\ \t\n\r]
$alphabetic = [a-zA-Z] -- alphabetic characters

$special   = [\(\)\,\;\[\]\`\{\}\']
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\?]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

@number     = [$digit]+
@identifier = ($alphabetic|_)($alphabetic|_|$digit)*


state :-
<0>                 "/*"                   { enterNewComment1 `andBegin` state_comment_1 }
<state_comment_1>   "*/"                   { leaveComment `andBegin` state_initial }
<state_comment_1>   .                      ;
<state_comment_1>   \n                     { skip }
<0>                 "//"                   { enterNewComment2 `andBegin` state_comment_2 }
<state_comment_2>   \n                     { leaveComment `andBegin` state_initial }
<state_comment_2>   .                      ;
<0>               $whitespace+             ;           
<0>               \"\"\"                  { enterNewString `andBegin` state_string2 }
<state_string2>    \\n                    { addCharToString '\n' }
<state_string2>    \\t                    { addCharToString '\t' }
<state_string2>    \\\^[@-_]              { addControlToString }
<state_string2>    \\$digit$digit$digit   { addAsciiToString }
<state_string2>    \"\"\"                 { leaveString `andBegin` state_initial }
<state_string2>    \\\"                   { addCharToString '\"' }
<state_string1>    \\\'                   { addCharToString '\'' }
<state_string2>    \\\\                   { addCharToString '\\' }
<state_string1>    \\                     { \_ _ -> lexerError "Illegal escape sequence" }
<state_string2>    \\[\ \n\t\f\r\b\v]+\\  ;
<state_string2>    \\\"\"\"               { leaveString `andBegin` state_initial }
<state_string2>    .                      { addCurrentToString }
<state_string2>    \n                     { skip }
<0>                \"                     { enterNewString `andBegin` state_string1}
<state_string1>    \\n                    { addCharToString '\n' }
<state_string1>    \\t                    { addCharToString '\t' }
<state_string1>    \\\^[@-_]              { addControlToString }
<state_string1>    \\$digit$digit$digit   { addAsciiToString }
<state_string1>    \\\"                   { addCharToString '\"' }
<state_string1>    \\\'                   { \_ _ -> lexerError "Illegal escape sequence" }
<state_string1>    \\\\                   { addCharToString '\\' }
<state_string1>    \\                     { \_ _ -> lexerError "Illegal escape sequence" }
<state_string1>    \\[\ \t\f\r\b\v]+\\   ;
<state_string1>    \"                     { leaveString `andBegin` state_initial }
<state_string1>    .                      { addCurrentToString }
<state_string1>    \n                     { \_ _ -> lexerError "String must be finished in one line " }
<0>               "class"                {mkL LCLASS}
<0>               "def"                  {mkL LDEF}
<0>               "extends"              {mkL LEXTENDS}
<0>               "if"                   {mkL LIF}
<0>               "elif"                 {mkL LELIF}
<0>               "else"                 {mkL LELSE}
<0>               "while"                {mkL LWHILE}
<0>               "return"               {mkL LRETURN}
<0>               "typecase"             {mkL LTYPECASE}
<0>               "and"                {mkL LAND}
<0>               "or"               {mkL LOR}
<0>               "not"             {mkL LNOT}
<0>               \+                     {mkL LPLUS}
<0>               \-                     {mkL LMINUS}
<0>               \*                     {mkL LTIMES}
<0>               \/                     {mkL LDIVIDE}
<0>               \=                     {mkL LGETS}
<0>               \=\=                   {mkL LEQUALS}
<0>               \<\=                   {mkL LATMOST}
<0>               \<                     {mkL LLESS}
<0>               \>\=                   {mkL LATLEAST}
<0>               \>                     {mkL LMORE}
<0>               \{                     {mkL LLBRACE}
<0>               \}                     {mkL LRBRACE}
<0>               \(                     {mkL LLPAREN}
<0>               \)                     {mkL LRPAREN}
<0>               \,                     {mkL LCOMMA}
<0>               \;                     {mkL LSEMICOLON}
<0>               \.                     {mkL LDOT}
<0>               \:                     {mkL LCOLON}
<0>               \%                     { \_ _ -> lexerError "Unexpected character" }
<0>               @identifier            {getVariable}
<0>               @number                {getInteger}
<0>               "--"\-*[^$symbol].*    { skip }
<0>               \n                     { skip }

{
-- Each action has type :: String -> Token

-- The token type:


data Lexeme = Lexeme AlexPosn LexemeClass (Maybe String)

instance Show Lexeme where 
    show (Lexeme _ EOF _)  = "  Lexeme EOF"
    show (Lexeme p cl mbs) = " Lexeme class=" ++ show cl ++ showap p ++ showst mbs
       where 
           showap pp = " posn=" ++ showPosn pp
           showst Nothing  = ""
           showst (Just s) = " string=" ++ show s

tokPosn :: Lexeme -> AlexPosn
tokPosn (Lexeme p _ _) = p

data LexemeClass =
    EOF |
    LID String |
    LINT Int  |
    LSTRINGLITERAL String |
    LCLASS |
    LDEF |
    LEXTENDS |
    LIF |
    LELIF |
    LELSE |
    LWHILE |
    LRETURN |
    LTYPECASE |
    LAND |
    LOR |
    LNOT |
    LGETS |
    LPLUS |
    LMINUS | 
    LTIMES |
    LDIVIDE |
    LEQUALS |
    LATMOST |
    LLESS |
    LATLEAST |
    LMORE |
    LLBRACE |
    LRBRACE |
    LLPAREN |
    LRPAREN |
    LCOMMA |
    LSEMICOLON |
    LDOT |
    LCOLON 
    deriving (Eq,Show)

mkL:: LexemeClass -> AlexInput -> Int -> Alex Lexeme 
mkL c (p, _, _, str) len= return (Lexeme p c (Just (take len str)))

--states

state_initial :: Int
state_initial = 0

-- actions

enterNewComment1, enterNewComment2, leaveComment , embedComment1, embedComment2, unembedComment1, unembedComment2 :: Action
enterNewString, leaveString, addAsciiToString, addCurrentToString, addControlToString  :: Action
getInteger, getVariable :: Action


enterNewComment1 input len= 
     do setLexerCommentDepth 1
        skip input len

enterNewComment2 input len= 
     do setLexerCommentDepth 1
        skip input len

embedComment1 input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len

embedComment2 input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len

unembedComment1 input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len

unembedComment2 input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)


setLexerCommentDepth:: Int -> Alex ()
setLexerCommentDepth ss= Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

leaveComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len



setLexerCommentValue:: String -> Alex ()
setLexerCommentValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentValue=ss}}, ())

getLexerCommentValue :: Alex String
getLexerCommentValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)


setLexerCommentState:: Bool -> Alex ()
setLexerCommentState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentState=ss}}, ())

data AlexUserState = AlexUserState
                   {
                       lexerCommentValue   :: String,
                       lexerCommentDepth   :: Int,
                       lexerCommentState   :: Bool,
                       lexerStringState    :: Bool,
                       lexerStringValue    :: String,
                       parserCollIdent     :: Map String Int,
                       parserCurrentToken  :: Lexeme,
                       parserPos          :: Pos
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                       lexerStringState = False,
                       lexerCommentDepth  = 0,
                       lexerStringValue   = "",
                       parserCollIdent    = Map.empty,
                       parserCurrentToken = Lexeme undefined EOF Nothing,
                       parserPos          = Nothing
                   }

enterNewString _     _   =
    do setLexerStringState True
       setLexerStringValue ""
       alexMonadScan

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())


addCharToComment :: Char -> Action
addCharToComment c _     _   =
    do addCharToLexerCommentValue c
       alexMonadScan

addCharToString :: Char -> Action
addCharToString c _     _   =
    do addCharToLexerStringValue c
       alexMonadScan

-- if we are given the special form '\^A'
addControlToString i@(_, _, _, input) len = addCharToString c' i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to 'addControlToString'"
    v = ord c
    c' = if (v >= 64)
            then chr (v - 64)
            else error "Invalid call to 'addControlToString'"


addAsciiToString i@(_, _, _, input) len = if (v < 256)
                                          then addCharToString c i len
                                          else lexerError ("Invalid ascii value : " ++ input)
  where
    s = if (len == 4)
           then drop 1 input
           else error "Invalid call to 'addAsciiToString'"
    r = readDec s
    v = if (length r == 1)
           then fst (head r)
           else error "Invalid call to 'addAsciiToString'"
    c = chr v

addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to addCurrentToString''"

addCharToLexerCommentValue :: Char -> Alex ()
addCharToLexerCommentValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentValue=c:lexerCommentValue (alex_ust s)}}, ())


addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())


getCollNameToIdent :: Alex (Map String Int)
getCollNameToIdent = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserCollIdent ust)

setCollNameToIdent :: Map String Int -> Alex ()
setCollNameToIdent ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserCollIdent=ss}}, ())

getParserCurrentToken :: Alex Lexeme
getParserCurrentToken = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserCurrentToken ust)

setParserCurrentToken :: Lexeme -> Alex ()
setParserCurrentToken ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserCurrentToken=ss}}, ())

getParserPos :: Alex Pos
getParserPos = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserPos ust)

setParserPos :: Pos -> Alex ()
setParserPos ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserPos=ss}}, ())

-- utilities

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

type Pos     = Maybe AlexPosn

line_number :: Pos -> (Int, Int)
line_number Nothing                   = (0, 0)
line_number (Just (AlexPn _ lig col)) = (lig, col)



leaveString (p, _, _, input) len =
    do s <- getLexerStringValue
       setLexerStringState False
       return (Lexeme p (LSTRINGLITERAL (reverse s)) (Just (take len input)))

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

getInteger (p, _, _, input) len = if (length r == 1)
                                  then return (Lexeme p (LINT (fst (head r))) (Just s))
                                  else lexerError "Invalid number"
  where
    s = take len input
    r = readDec s

getVariable (p, _, _, input) len = return (Lexeme p (LID s) (Just s))
  where
    s = take len input


-- definition needed by Alex
alexEOF :: Alex Lexeme
alexEOF = return (Lexeme undefined EOF Nothing)
-- Execution
scanner :: String -> Either String [Lexeme]
scanner str = let loop = do (t, m) <- alexComplementError alexMonadScan
                            when (isJust m) (lexerError (fromJust m))
                            let tok@(Lexeme _ cl _) = t
                            if (cl == EOF)
                               then do f1 <- getLexerStringState
                                       d2 <- getLexerCommentDepth
                                       if ((not f1) && (d2 == 0))
                                          then return [tok]
                                          else if (f1)
                                               then alexError "String not closed at end of file"
                                               else alexError "Comment not closed at end of file"
                               else do toks <- loop
                                       return (tok : toks)
              in  runAlex str loop


-- we capture the error message in order to complement it with the file position
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) = Alex (\s -> case al s of
                                                 Right (s', x) -> Right (s', (x, Nothing))
                                                 Left  message -> Right (s, (undefined, Just message)))


lexer :: (Lexeme -> Alex a) -> Alex a
lexer cont =
    do t <- lexToken
       setParserCurrentToken t  -- helps in producing informative error messages
       cont t


lexToken :: Alex Lexeme
lexToken =
    do
       inp <- alexGetInput
       sc <- alexGetStartCode
       case alexScan inp sc of
            AlexEOF              -> alexEOF
            AlexError _          -> alexError "lexical error"
            AlexSkip  inp1 _     -> do
                                       alexSetInput inp1
                                       lexToken
            AlexToken inp1 len t -> do
                                       alexSetInput inp1
                                       t inp len
lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
       let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
       let inp2 = if (length inp1 > 30)
                     then trim (take 30 inp1)
                     else trim inp1
       let disp = if (null inp)
                     then " at end of file"
                     else if (null inp2)
                             then " before end of line\n"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'\n"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ "\n at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

type Action = AlexInput -> Int -> Alex Lexeme
-- used by the parser: run lexer, parser & get the symbol table
runAlexTable :: String -> Alex a -> Either String (a, Map String Int)
runAlexTable input (Alex f)
   = case f (AlexState { alex_pos = alexStartPos
                       , alex_inp = input
                       , alex_chr = '\n'
                       , alex_scd = 0
                       , alex_ust = alexInitUserState }) of
            Left msg      -> Left msg
            Right (st, a) -> Right (a, parserCollIdent (alex_ust st))
data Flag
     =
       Reject
        deriving (Show, Eq)
options :: [OptDescr Flag]
options =
    [
    ]
execOpts :: IO ([Flag], [String])
execOpts =
    do argv <- getArgs
       progName <- getProgName
       let header = "Usage: " ++ progName ++ " [options...] \"file name\""
       case (getOpt Permute options argv) of
            (o, n, []  ) -> if ((Reject `elem` o) || (length n /= 1))
                           then error (usageInfo header options)
                           else return (o, n)
            (_, _, errs) -> error (concat errs ++ usageInfo header options)

main_Lexer :: IO ()
main_Lexer =
    do (_, fileList) <- execOpts
       let filename = head fileList
       flag <- doesFileExist filename
       when (not flag) (error ("The following file does not exist : " ++ filename))
       putStrLn ("Beginning Scanning of the Quack program in file " ++ head fileList)
       s <- readFile filename
       let sr = scanner s
       case sr of
            Left st  -> error st
            Right ls -> putStrLn (show ls)
}
 