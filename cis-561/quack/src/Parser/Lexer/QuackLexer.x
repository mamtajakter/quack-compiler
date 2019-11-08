{

{-# OPTIONS -w -funbox-strict-fields #-}

module Parser.Lexer.QuackLexer ( mainLexer
                  , lexer, Lexeme (..), LexemeClass (..)
                  , Alex, getParserCurrentToken
                  , alexError, runAlex, runAlexTable, alexGetInput, scanner
                  , Flag (..), options, execOpts, error', showPosn
                  ) where

import Prelude hiding ( GT, LT, EQ )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), getOpt, usageInfo )
import System.Environment ( getArgs, getProgName )
import System.Directory ( doesFileExist )
import Control.Monad
import Data.Maybe
import Numeric ( readDec )
import Data.Char ( chr )
import Data.Map ( Map )
import Data.IORef (readIORef, newIORef, writeIORef)
import qualified Data.Map as Map ( empty )

}


%wrapper "monadUserState"

$whitechar = [ \t\n\r]
$special   = [\(\)\,\;\[\]\`\{\}\\]
$digit     = 0-9
$large     = [A-Z]
$small     = [a-z \_]
$alpha     = [$small $large]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\']
$char1     = \*
$char2     = \/
@tocheck   = $char1$char2
@varid     = [_ $alpha]+[$alpha $digit]*
@varcomment = [varid $ascsymbol]+
@number     = [$digit]+




state:-


<0>             \"           { enterNewString1 `andBegin` state_string_1 }
<state_string_1>  \"           { leaveString1 `andBegin` initial_state }
<state_string_1>   \\n  {addCharToString '\n' }
<state_string_1>  \\\,     {\_ _ -> lexerError "Illegal escape sequence"}
<state_string_1>  \\\.    {\_ _ -> lexerError "Illegal escape sequence" }
<state_string_1>    .          {addCurrentToString}
<state_string_1>    \n        {\_ _ -> lexerError "Illegal escape sequence"}
<state_string_1>    [~\n]      ;
<0>             "//".*       ;
<0>             "/*"         {enterNewComment2 `andBegin` state_comment_2}
<state_comment_2> .           ;
<state_comment_2> \n          {skip}
<state_comment_2>  "*/"       {leaveComment2 `andBegin` initial_state}
<0>            "class"       { mkL LCLASS }
<0>            "def"         { mkL LDEF }
<0>            "extends"     { mkL LEXTENDS }
<0>            "if"          { mkL LIF}
<0>            "elif"        { mkL LELIF }
<0>            "else"        { mkL LELSE }
<0>            "while"       { mkL LWHILE }
<0>            "return"      { mkL LRETURN }
<0>            "typecase"    { mkL LTYPECASE }
<0>             "or"         { mkL LOR }
<0>             "and"        { mkL LAND }
<0>             "not"        { mkL LNOT }
<0>             "True"       { mkL LTRUE }
<0>             "False"      { mkL LFALSE }
<0>             \>\=         { mkL LGE }
<0>             \>           { mkL LGT }
<0>             \<\=         { mkL LLE }
<0>             \<           { mkL LLT }
<0>             \=           { mkL LLEQ }
<0>             \=\=         { mkL LEQUALS }
<0>             \/           { mkL LDIVIDE }
<0>             \*           { mkL LTIMES }
<0>             \-           { mkL LMINUS }
<0>             \+           { mkL LPLUS }
<0>             \.           { mkL LDOT }
<0>             \}           { mkL LBRACER }
<0>             \{           { mkL LBRACEL }
<0>             \)           { mkL LPARENR }
<0>             \(           { mkL LPARENL }
<0>             \;           { mkL LSEMICOLON }
<0>             \:           { mkL LCOLON }
<0>             ","          { mkL LCOMMA }


-- <state_string_1>  \\n          { addCharToString '\n' }
-- <state_string_1>  \\t          { addCharToString '\t' }
-- -- <state_string_1>  \\0          { addCharToString '\0'}
-- <state_string_1>  \\b          { addCharToString '\b'}
-- <state_string_1>  \\r          { addCharToString '\r'}
-- <state_string_1>  \\f          { addCharToString '\f'}
-- <state_string_1>  \\\"         { addCharToString '\"'}
--
-- <state_string_1>  \\\n         { \_ _ -> lexerError "Illegal escape sequence"}
<0>               \"\"\"       { enterNewString2 `andBegin` state_string_2}
<state_string_2>    .          {addCurrentToString}
<state_string_2>    \n        {addCharToString '\n'}
<state_string_2>  [~\"\"\"]    ;
<state_string_2>   \"\"\"      { leaveString2 `andBegin` initial_state }
<0>                $white      ;
<0>                @number     { getInteger }
<0>                @varid      { getVariable }



{


data Lexeme = Lexeme LexemeClass AlexPosn (Maybe String)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,_,str) len = return (Lexeme c p (Just (take len str)))

data LexemeClass =
           EOF
           | LID         String
           | LINT        Int
           | LSTRING     String
           | LCLASS
           | LDEF
           | LEXTENDS
           | LIF
           | LELIF
           | LELSE
           | LWHILE
           | LRETURN
           | LTYPECASE
           -- | LRESERVEDID
           -- | LRESERVEDOP
           | LOR
           | LGE
           | LGT
           | LLE
           | LLT
           | LLEQ
           | LEQUALS
           | LAND
           | LNOT
           | LDIVIDE
           | LTIMES
           | LMINUS
           | LPLUS
           | LDOT
           | LBRACER
           | LBRACEL
           | LPARENL
           | LPARENR
           | LSEMICOLON
           | LCOLON
           | LCOMMA
           | LFALSE
           | LTRUE
           deriving (Show, Eq)

instance Show Lexeme where
  show (Lexeme EOF _ _)   = "Lexeme EOF"
  show (Lexeme cl  p mbs) = "Lexeme class=" ++ show cl ++ " " ++ showPosn p ++ " " ++ showst mbs ++ " "
    where
      showst Nothing  = ""
      showst (Just s) = " string=" ++ "\'" ++ s ++ "\'"


-- The user state monad
data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentState1  :: Bool
                     , lexerCommentState2  :: Bool
                     , lexerStringState1   :: Bool
                     , lexerStringState2   :: Bool
                     , lexerStringValue   :: String
                     -- used by the parser phase
                     , parserCurrentToken :: Lexeme
                   }
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                   -- used by the lexer phase
                       lexerCommentState1  = False
                     , lexerCommentState2  = False
                     , lexerStringState1   = False
                     , lexerStringState2   = False
                     , lexerStringValue   = ""
                     -- used by the parse phase
                     , parserCurrentToken = Lexeme EOF undefined Nothing
                   }

initial_state :: Int
initial_state = 0



enterNewComment1 _ _ = do
  setLexerCommentState1 True
  alexMonadScan

enterNewComment2 _ _ = do
  setLexerCommentState2 True
  alexMonadScan

leaveComment1 (_, _, _, input) len =
   do
      setLexerCommentState1 False
      alexMonadScan

leaveComment2 (_, _, _, input) len =
   do
      setLexerCommentState2 False
      alexMonadScan



setLexerCommentState1 :: Bool -> Alex ()
setLexerCommentState1 b = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentState1=b}}, ())


setLexerCommentState2 :: Bool -> Alex ()
setLexerCommentState2 b = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentState2=b}}, ())


getLexerCommentState1 :: Alex Bool
getLexerCommentState1 = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentState1 ust)

getLexerCommentState2 :: Alex Bool
getLexerCommentState2 = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState2 ust)

enterNewString1 _     _   =
    do setLexerStringState1 True
       setLexerStringValue ""
       alexMonadScan

leaveString1 (p, _, _, input) len =
   do s <- getLexerStringValue1
      setLexerStringState1 False
      return (Lexeme (LSTRING (reverse s)) p (Just (take len input)))

enterNewString2 _     _   =
   do setLexerStringState2 True
      setLexerStringValue ""
      alexMonadScan

leaveString2 (p, _, _, input) len =
   do s <- getLexerStringValue2
      setLexerStringState2 False
      return (Lexeme (LSTRING (reverse s)) p (Just (take len input)))


getLexerStringValue1 :: Alex String
getLexerStringValue1 = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

getLexerStringValue2 :: Alex String
getLexerStringValue2 = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue1 :: String -> Alex ()
setLexerStringValue1 ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())


setLexerStringState1 :: Bool -> Alex ()
setLexerStringState1 b = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState1=b}}, ())

setLexerStringState2 :: Bool -> Alex ()
setLexerStringState2 b = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState2=b}}, ())


getLexerStringState1 :: Alex Bool
getLexerStringState1 = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState1 ust)


getLexerStringState2 :: Alex Bool
getLexerStringState2 = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState2 ust)

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())


addCharToString :: Char -> Action
addCharToString c _ _ =
   do addCharToLexerStringValue c
      alexMonadScan

addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to addCurrentToString''"

getInteger (p, _, _, input) len = if (length r == 1)
                                  then return (Lexeme (LINT (fst (head r))) p (Just s))
                                  else lexerError "Invalid number"
  where
    s = take len input
    r = readDec s

-- a sequence of letters is an identifier, except for reserved words, which are tested for beforehand
getVariable (p, _, _, input) len = return (Lexeme (LID s) p (Just s))
  where
    s = take len input



setParserCurrentToken :: Lexeme -> Alex ()
setParserCurrentToken ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserCurrentToken=ss}}, ())


getParserCurrentToken :: Alex Lexeme
getParserCurrentToken = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserCurrentToken ust)


-- definition needed by Alex
alexEOF :: Alex Lexeme
alexEOF = return (Lexeme EOF undefined Nothing)

scanner' :: Either String [Lexeme] -> [Either String Lexeme] -> [Either String Lexeme]
scanner' e@(Left s) [] = [Left s]
scanner' e@(Left s) l@(x:xs) = l ++ [Left s]
scanner' e@(Right lexlist) [] = map (\lex -> Right lex) lexlist
scanner' e@(Right lexlist) l@(x:xs) = l ++ (map (\lex -> Right lex) lexlist)

-- Execution
scanner :: String -> Either String [Lexeme]
scanner str = let loop = do
                          (t, m) <- alexComplementError alexMonadScan
                          when (isJust m) (lexerError (fromJust m))
                          let tok@(Lexeme cl _ _) = t
                          if (cl == EOF)
                                  then do
                                        f1 <- getLexerStringState1
                                        f2 <- getLexerStringState2
                                        d2 <- getLexerCommentState2
                                        if ((not f1) && (not f2) && (not d2))
                                         then return [tok]
                                          else if (f1)
                                              then alexError "Simple String not closed at end of file"
                                                else if (f2)
                                                 then alexError "Complex String not closed at end of file"
                                                  else alexError "Multiline comment not closed at end of file"
                                else do
                                       toks <- loop
                                       return (tok : toks)
            in  runAlex str loop

{- scanner :: String -> Either String [Lexeme]
scanner str = let loop = do
                          (t, m) <- alexComplementError alexMonadScan
                          when (isJust m) (lexerError (fromJust m))
                          let tok@(Lexeme cl _ _) = t
                          if (cl == EOF)
                                  then do
                                        f1 <- getLexerStringState1
                                        f2 <- getLexerStringState2
                                        d2 <- getLexerCommentState2
                                        if ((not f1) && (not f2) && (not d2))
                                         then return [tok]
                                          else if (f1)
                                              then alexError "Simple String not closed at end of file"
                                                else if (f2)
                                                 then alexError "Complex String not closed at end of file"
                                                  else alexError "Multiline comment not closed at end of file"
                                else do
                                       toks <- loop
                                       return (tok : toks)
            in  runAlex str loop -}



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

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = " Line " ++ show line ++ ", Column " ++ show col

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
                           then " before end of line"
                           else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
     let disp3 = if (null msg)
                    then "Lexer error"
                    else trim msg
     alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
   trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')



type Action = AlexInput -> Int -> Alex Lexeme
-- used by the parser: run lexer, parser & get the symbol table
runAlexTable :: String -> Alex a -> Either String a
runAlexTable input (Alex f)
 = case f (AlexState { alex_pos = alexStartPos
                     , alex_inp = input
                     , alex_chr = '\n'
                     , alex_scd = 0
                     , alex_ust = alexInitUserState }) of
          Left msg      -> Left msg
          Right (st, a) -> Right a


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


error' = error


mainLexer :: String ->  IO [Either String Lexeme]
mainLexer filename =
  do
     flag <- doesFileExist filename
     when (not flag) (error ("The following file does not exist : " ++ filename))
     putStrLn ("Beginning analysis of the Quack program in file " ++ filename)
     s <- readFile filename
     let sr = scanner s
     let sr' = scanner' sr []
     return sr'
     -- putStrLn $ unlines $ map show sr' --putStrLn (show ls)
}
