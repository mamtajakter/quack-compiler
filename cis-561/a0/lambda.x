{

  {-# OPTIONS -w -funbox-strict-fields #-}
  {-# Language GADTs #-}
module LambdaCalcLexer ( main
                  , lexer, Lexeme (..), LexemeClass (..), tokPosn
                  , Pos, Alex, getCollNameToIdent, getParserCurrentToken, setCollNameToIdent
                  , getParserPos, setParserPos
                  , alexError, runAlex, runAlexTable, alexGetInput, showPosn
                  , line_number
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
import qualified Data.Map as Map ( empty )

}

%wrapper "monadUserState"

$whitespace = [\ \t\b\n]
$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z]
$letter     = [a-zA-Z]                                       -- alphabetic characters
$ident      = [$letter $digit _]                             -- identifier character
@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*




state :-
  <0>            $whitespace ;
  <0>             "."        {skip}
  <0>           "lambda"     {mkL Lambda}
  <0>            "("         {mkL LParen}
  <0>            ")"         {mkL RParen}
  <0>            $digit      {getInteger}
  <0>            @identifier {getVariable}







{

data Lexeme = Lexeme AlexPosn LexemeClass (Maybe String)

instance Show Lexeme where
 show (Lexeme _ EOF _)   = "  Lexeme EOF"
 show (Lexeme p cl  mbs) = "  Lexeme class=" ++ show cl ++ showap p ++ showst mbs
  where
    showap pp = " posn=" ++ showPosn pp
    showst Nothing  = ""
    showst (Just s) = " string=" ++ show s

data LexemeClass where
   EOF    :: LexemeClass
   Ident  :: String -> LexemeClass
   Const  :: Integer -> LexemeClass
   Lambda :: LexemeClass
   App    :: LexemeClass -> LexemeClass -> LexemeClass
   LParen :: LexemeClass
   RParen :: LexemeClass
   deriving (Show, Eq)

tokPosn :: Lexeme -> AlexPosn
tokPosn (Lexeme p _ _) = p


mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p, _, _, str) len = return (Lexeme p c (Just (take len str)))

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

-- states
state_initial :: Int
state_initial = 0

type Action = AlexInput -> Int -> Alex Lexeme


enterNewComment, embedComment, unembedComment :: Action
unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len

enterNewString, addCurrentToString :: Action

enterNewComment input len =
    do setLexerCommentDepth 1
       skip input len
embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len
enterNewString _     _   =
    do setLexerStringState True
       setLexerStringValue ""
       alexMonadScan
addCharToString :: Char -> Action
addCharToString c _     _   =
    do addCharToLexerStringValue c
       alexMonadScan
addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to addCurrentToString''"


getInteger (p, _, _, input) len = if (length r == 1)
                                 then return (Lexeme p (Const (fst (head r))) (Just s))
                                 else lexerError "Invalid number"
 where
   s = take len input
   r = readDec s


-- a sequence of letters is an identifier, except for reserved words, which are tested for beforehand
getVariable (p, _, _, input) len = return (Lexeme p (Ident s) (Just s))
 where
   s = take len input


-- The user state monad
data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int
                     , lexerStringState   :: Bool
                     , lexerStringValue   :: String
                     -- used by the parser phase
                     , parserCollIdent    :: Map String Int
                     , parserCurrentToken :: Lexeme
                     , parserPos          :: Pos
                   }
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                       lexerCommentDepth  = 0
                     , lexerStringState   = False
                     , lexerStringValue   = ""
                     , parserCollIdent    = Map.empty
                     , parserCurrentToken = Lexeme undefined EOF Nothing
                     , parserPos          = Nothing
                   }


getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)
setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())
getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)
setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())
getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)
setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())
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



type Pos     = Maybe AlexPosn
line_number :: Pos -> (Int, Int)
line_number Nothing                   = (0, 0)
line_number (Just (AlexPn _ lig col)) = (lig, col)
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
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')



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

main :: IO ()
main =
  do (_, fileList) <- execOpts
     let filename = head fileList
     flag <- doesFileExist filename
     when (not flag) (error ("The following file does not exist : " ++ filename))
     putStrLn ("Analysing Lambda calc file: " ++ head fileList)
     s <- readFile filename
     let sr = scanner s
     case sr of
          Left st  -> error st
          Right ls -> putStrLn (show ls)
}
