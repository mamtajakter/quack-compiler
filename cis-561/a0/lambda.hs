{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "lambda.x" #-}


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


#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
{-# LINE 1 "templates/wrappers.hs" #-}



















































































































































































-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.


import Control.Applicative as App (Applicative (..))
import qualified Control.Monad (ap)


import Data.Word (Word8)


import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))








-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad


data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int        -- the current startcode

      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program

    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either String a
runAlex input (Alex f)
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_bytes = [],

                        alex_ust = alexInitUserState,

                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp} ->
        Right (s, (pos,c,bs,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())


alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())


alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp' _len -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> Alex result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip _input _len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code _input _len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input len = return (t input len)



-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)




-- -----------------------------------------------------------------------------
-- Basic wrapper




-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version






-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.




-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version




-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.


alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0,12) [-7,0,0,68,0,0,0,143,218,293,368,443,518]

alex_table :: Array Int Int
alex_table = listArray (0,773) [0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,5,0,0,0,0,2,0,6,6,6,6,6,6,6,6,6,6,0,0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,10,10,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,10,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,10,0,3,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,10,0,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,10,0,8,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,10,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,10,0,10,12,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,10,0,10,10,10,7,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,773) [-1,8,9,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,32,-1,-1,-1,-1,-1,-1,-1,40,41,-1,-1,-1,-1,46,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,-1,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,12) [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,12) [AlexAccNone,AlexAccSkip,AlexAcc 10,AlexAcc 9,AlexAcc 8,AlexAcc 7,AlexAcc 6,AlexAcc 5,AlexAcc 4,AlexAcc 3,AlexAcc 2,AlexAcc 1,AlexAcc 0]

alex_actions = array (0::Int,11) [(10,alex_action_1),(9,alex_action_2),(8,alex_action_3),(7,alex_action_4),(6,alex_action_5),(5,alex_action_6),(4,alex_action_6),(3,alex_action_6),(2,alex_action_6),(1,alex_action_6),(0,alex_action_6)]

{-# LINE 54 "lambda.x" #-}


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

alex_action_1 = skip
alex_action_2 = mkL Lambda
alex_action_3 = mkL LParen
alex_action_4 = mkL RParen
alex_action_5 = getInteger
alex_action_6 = getVariable
{-# LINE 1 "templates/GenericTemplate.hs" #-}



















































































































































































-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine






alexIndexInt16OffAddr arr off = arr ! off



alexIndexInt32OffAddr arr off = arr ! off



quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
  (AlexNone, input') ->
    case alexGetByte input of
      Nothing ->



                                   AlexEOF
      Just _ ->



                                   AlexError input'

  (AlexLastSkip input'' len, _) ->



    AlexSkip input'' len

  (AlexLastAcc k input''' len, _) ->



    AlexToken input''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input of
     Nothing -> (new_acc, input)
     Just (c, new_input) ->



      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input (len)

        check_accs (AlexAccPred a predx rest)
           | predx user orig_input (len) input
           = AlexLastAcc a input (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user orig_input (len) input
           = AlexLastSkip input (len)
           | otherwise
           = check_accs rest


data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip

  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

alexPrevCharMatches f _ input _ _ = f (alexInputPrevChar input)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input =
     case alex_scan_tkn user input (0) input sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.

