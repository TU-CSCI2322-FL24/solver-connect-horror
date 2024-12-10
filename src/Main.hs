module Main where

import Solver
import ReadWrite
import Connect4
import System.IO
import System.Environment
import System.Directory
import System.Console.GetOpt
import Data.Maybe
import Text.Read

data Flag = Win | Help | Depth String| Move String | Verbose | Interactive deriving (Show,Eq)

options =
    [ Option ['w'] ["win"] (NoArg Win) "Find the best move with exhaustive search (no cut-off depth)"
    , Option ['h'] ["help"] (NoArg Help) "Print usage information and exit"
    , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Set AI foresight to <num>"
    , Option ['m'] ["move"] (ReqArg Move "<num>") "Make a move on column <num> and print out the new board"
    , Option ['v'] ["verbose"] (NoArg Verbose) "Print with more information"
    , Option ['i'] ["interactive"] (NoArg Interactive) "Play a game against the computer"
    ]

-- asks for a file name and returns the game, the best move, and the outcome of that move
main :: IO()
main =
   do args <- getArgs
      let (flags,inputs,errors) = getOpt Permute options args
      putStrLn $ show (flags,inputs,errors)
      if Help `elem` flags
      then putStrLn $ usageInfo "Connect4 [options] [filename] Connect 4 Solver." options
      else
        do
          -- chatGPT used to help with putting folders in the right place and getting correct path
          let folderPath = "../testFiles"
              fName = head inputs
          --fName <- prompt "Enter a file name"
          let fullPath = folderPath ++ "/" ++ fName
          game <- loadGame fullPath
          --putStrLn $ prettyPrint game
          dispatch flags game
          --putBestMove game

dispatch flags game 
  | Win `elem` flags = if Verbose `elem` flags then moveToEnd game True else moveToEnd game False
  | any isDepth flags = if Verbose `elem` flags then putBestMove game (getDepth flags) True else putBestMove game (getDepth flags) False
  | any isMove flags = if Verbose `elem` flags then putMove game (getMove flags) True else putMove game (getMove flags) False
  | Interactive `elem` flags = if Verbose `elem` flags 
                               then if any isDepth flags 
                                    then playGame game (getDepth flags) True 
                                    else playGame game defaultDepth True
                               else if any isDepth flags 
                                    then playGame game (getDepth flags) False
                                    else playGame game defaultDepth False
  | otherwise = if Verbose `elem` flags then putBestMove game defaultDepth True else putBestMove game defaultDepth False
  

-- isMove
isMove :: Flag -> Bool
isMove (Move _) = True
isMove _ = False

getMove :: [Flag] -> Int
getMove [] = 10
getMove (Move x:_) = read x
getMove (_:flags) = getDepth flags

-- isDepth
isDepth :: Flag -> Bool
isDepth (Depth _) = True
isDepth _ = False

getDepth :: [Flag] -> Int
getDepth [] = 10
getDepth (Depth x:_) = read x
getDepth (_:flags) = getDepth flags

-- default num for depth
defaultDepth :: Int
defaultDepth = 5

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


putMove :: Game -> Int -> Bool -> IO()
putMove game move False = putStrLn (showGame $ makeMove game move)
putMove game move True = putStrLn (prettyPrint (makeMove game move))

putBestMove :: Game -> Int -> Bool -> IO()
putBestMove game depth v = 
  let (rating, mMove) = whoMightWin game depth
      move = fromMaybe (-1) mMove
  in if move == (-1) 
     then putStr "\n invalid move error\n"
     else if v then putStrLn $ prettyPrint game ++ "\nThe best move is to plave the piece in column " ++ show move ++ ". This will put the board in favor of " ++ decipherRating rating ++ "."
               else putStrLn $ "The best move is to plave the piece in column " ++ show move ++ ". This will put the board in favor of " ++ decipherRating rating ++ "."

decipherRating :: Rating -> String
decipherRating rating 
  | rating < 0 = "the Red player"
  | rating > 0 = "the Yellow player"
  | otherwise  = "neither player (neutral/tied game state)"
-- takes a game and recurses until a win or game ends
moveToEnd :: Game -> Bool -> IO()
moveToEnd game v =
  let (mMove, winner) = bestMove game
      winnerStr = winnerToString winner
      move = fromMaybe (-1) mMove
  in if move == -1
     then putStr "\n invalid move error\n" 
     else if v 
          then putStrLn $ prettyPrint game ++ "\nThe best move is to place the piece in column " ++ show move ++ ". This will force a " ++ winnerStr ++ ".\n"
          else putStr $ "\nBest move is column " ++ show move

playGame :: Game -> Int -> Bool -> IO()
playGame (p,b) depth True 
  | p == Red = 
              let (rating, move) = whoMightWin (p,b) depth
                  parsedMove = fromMaybe 0 move
                  newBoard = makeMove (p,b) parsedMove
              in if parsedMove == 0 
                then putStrLn $ "\nThe winner is " ++ decipherRating rating ++ "!!\n\n"
                else do putStrLn $ "\n_________________________________________________\n\n\nthe computers move is " ++ show (fromJust move) ++ ".\n" -- ++ prettyPrint newBoard
                        playGame newBoard depth True
  | p == Yellow = 
              do  putStrLn $ prettyPrint (p,b)
                  if isJust (wonGame b)
                  then putStrLn $ "\nThe winner is " ++ show (fromJust (wonGame b)) ++ "!!\n\n"
                  else do moveS <- prompt "What column do you want to move in?\n"
                          let move = fromMaybe 0 (readMaybe moveS)
                           in if move == 0 || not (checkMove b move)
                              then do putStrLn "\n\n\n\nnot a valid move"
                                      playGame (p,b) depth True
                              else let newboard = makeMove (p,b) move
                                   in do putStrLn $ "\n_________________________________________________\n\n\n" ++ prettyPrint newboard
                                         playGame newboard depth True
playGame (p,b) depth False 
  | p == Red = 
              let (rating, move) = whoMightWin (p,b) depth
                  parsedMove = fromMaybe 0 move
                  newBoard = makeMove (p,b) parsedMove
              in if parsedMove == 0 
                then putStrLn $ "\nThe winner is " ++ decipherRating rating ++ "!!\n\n"
                else do putStrLn "\n_________________________________________________\n\n\n"
                        playGame newBoard depth False
  | p == Yellow = 
              do  putStrLn $ prettyPrint (p,b)
                  if isJust (wonGame b)
                  then putStrLn $ "\nThe winner is " ++ show (fromJust (wonGame b)) ++ "!!\n\n"
                  else do moveS <- prompt "What column do you want to move in?\n"
                          let move = fromMaybe 0 (readMaybe moveS)
                           in if move == 0 || not (checkMove b move)
                              then do putStrLn "\n\n\n\nnot a valid move"
                                      playGame (p,b) depth False
                              else let newboard = makeMove (p,b) move
                                   in do putStrLn $ "\n_________________________________________________\n\n\n" ++ prettyPrint newboard
                                         playGame newboard depth False
                  

-- helper function to convert the winner data type to a string
winnerToString :: Winner -> String
winnerToString Tie = "tie"
winnerToString (Winner Red) = "win by red"
winnerToString (Winner Yellow) = "win by yellow"
