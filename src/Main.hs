module Main where

import Connect4
import System.IO
import System.Environment
import System.Directory
import System.Console.GetOpt
import Data.Attoparsec.ByteString.Char8 (Number(D))

data Flag = Win | NA | Depth String deriving (Show,Eq)

options =
    [ Option ['w'] ["win"] (NoArg Win) "Find the best move with exhaustive search (no cut-off depth)"
    , Option ['n'] ["NA"] (NoArg NA) "Temp flag while empty"
    , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Set AI foresight to <num>"
    ]

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
              fName = head inputs
          --fName <- prompt "Enter a file name"
          let fullPath = folderPath ++ "/" ++ fName
          game <- loadGame fullPath
          putStrLn $ prettyPrint game
          dispatch flags game
          --putBestMove game

dispatch flags game 
  | Win `elem` flags = putBestMove game 42
  | any isNumber flags = putBestMove game (getNumber flags)
  | otherwise = putBestMove game defaultDepth
  

-- isNumber
isNumber :: Flag -> Bool
isNumber (Depth _) = True
isNumber _ = False

getNumber:: [Flag] -> Int
getNumber [] = 10
getNumber (Depth x:_) = read x
getNumber (_:flags) = getNumber flags

-- default num for depth
defaultDepth :: Int
defaultDepth = 10

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
putBestMove :: Game -> Int -> IO()
putBestMove game depth =
  let (move, winner) = bestMove game depth
      winnerStr = winnerToString winner
  in putStr $ "\nThe best move is to place the piece in column " ++ show move ++ ". This will force a " ++ winnerStr ++ ".\n"


-- helper function to convert the winner data type to a string
winnerToString :: Winner -> String
winnerToString Tie = "tie"
winnerToString (Winner Red) = "win by red"
winnerToString (Winner Yellow) = "win by yellow"
