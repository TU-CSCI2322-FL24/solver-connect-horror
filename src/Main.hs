module Main where

import Connect4
import System.IO
import System.Environment
import System.Directory
import System.Console.GetOpt

data Flag = NA deriving (Show,Eq)

options :: [OptDescr Flag]
options = [Option ['n'] ["NA"] (NoArg NA) "Temp flag while empty"]

-- asks for a file name and returns the game, the best move, and the outcome of that move
main :: IO()
main =
   do args <- getArgs
      let (flags,inputs,errors) = getOpt Permute options args
      putStrLn $ show (flags,inputs,errors)
      if NA `elem` flags
      then print "temp placeholder"
      else
        do
          -- chatGPT used to help with putting folders in the right place and getting correct path
          let folderPath = "../testFiles"
          fName <- prompt "Enter a file name"
          let fullPath = folderPath ++ "/" ++ fName
          game <- loadGame fullPath
          putStrLn $ prettyPrint game
          dispatch flags game
          --putBestMove game

dispatch flags game 
  | otherwise = putBestMove game
-- helper function to ask for user input 
prompt :: String -> IO String
prompt str =
  do putStr $ str ++ ": "
     hFlush stdout
     answer <- getLine
     return answer

-- takes a game and file path and puts the game in the file
writeGame :: Game -> FilePath -> IO ()
writeGame game file =
  let stringGame = showGame game
  in writeFile file stringGame

-- takes a file and returns the game in the file
loadGame :: FilePath -> IO Game
loadGame file =
  do
  contents <- readFile file
  return $ readGame contents

-- takes a game and returns the best move with the outcome
putBestMove :: Game -> IO()
putBestMove game =
  let (move, winner) = bestMove game
      winnerStr = winnerToString winner
  in putStr $ "\nThe best move is to place the piece in column " ++ show move ++ ". This will force a " ++ winnerStr ++ ".\n"

-- helper function to convert the winner data type to a string
winnerToString :: Winner -> String
winnerToString Tie = "tie"
winnerToString (Winner Red) = "win by red"
winnerToString (Winner Yellow) = "win by yellow"
