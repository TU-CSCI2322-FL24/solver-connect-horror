module Main where

import Connect4
import System.IO
import System.Environment
import Text.Read
import Data.Char

main :: IO() 
main =
   do args <- getArgs
      fName <- prompt "Enter a file name"
      -- contents <- readFile fName
      loadGame fName

prompt :: String -> IO String
prompt str = 
  do putStr $ str ++ ": "
     hFlush stdout
     answer <- getLine
     return answer

loadGame :: FilePath -> IO Game 
loadGame file = 
  do 
	contents <- readFile file
	return $ readGame contents
