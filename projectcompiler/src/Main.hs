module Main where


import System.Directory
import Parser
import Control.Monad

filedirectory:: String
filedirectory = "samples/GoodWalk.qk"

main :: IO ()
main = main_Parser filedirectory
