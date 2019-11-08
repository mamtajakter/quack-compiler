module Main where

import Parser.QuackParser
import Parser.Lexer.QuackLexer
import System.Directory
import System.Environment (getArgs)
import System.IO
import TypeChecker.TypeCheck
import CodeGenerator.Generator
import qualified Data.Map as M -- for ghci 
import TypeChecker.QuackTypes  -- for ghci 
import Data.Maybe (fromJust)

uneither :: (Show a, Show b) => Either a b -> String
uneither (Left a)  = show a
uneither (Right b) = show b


filepath :: String
filepath = "samples/SqrBadSyntax.qk"

main :: IO ()
main = do
  files      <- getArgs
  if length files /= 2
     then putStrLn "Usage: quack <input-file> <output-file>"
     else
       do
        let input_file  = head files
        let output_c_file = last files
        handle     <- openFile output_c_file ReadWriteMode
        progResult <- mainParse input_file
        case runParser progResult startState of
          Left msg   -> do
            putStrLn ">>> File rejected!  "
            print msg
          Right (prog, pstate)  -> do
            putStrLn ">>> File parsed correctly"
            putStrLn ">>> Type checking program ..."
            case typeCheckProg prog of
              Left err -> do
                putStrLn ">>> Failed to type check"
                print err
                print prog
              Right (ctable1, cvartable1, tyctx1) -> do
                putStrLn "Program type checks!"
                print prog 
                let startdata = empty_cdGen{ctable=ctable1, cvartable=cvartable1, tyctx=tyctx1}
                dataResult <- genProg (return startdata) prog
                let (d, state) = runGen dataResult startdata
                putStrLn ("See " ++ output_c_file ++ " for generated c code")
                hPutStr handle (code d) >>= (\s -> hClose handle)
