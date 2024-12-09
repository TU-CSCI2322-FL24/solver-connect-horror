{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromMaybe" #-}
module Connect4 where

import Data.Maybe 
import Debug.Trace

--                                       Data Declaration

--Player is the token color of each player, either Red or Yellow
data Player = Red | Yellow deriving (Eq, Show, Ord)

--Position is the value in each spot of the game boared, either a player token, or an empty space
data Position = Player Player | Empty deriving (Eq, Show, Ord)

--Winner is either the Player and the set of moves they won with, or a tied board state where every slot is filled
--and there is no winning set of moves
--QUESTION: instead of having a tuple (Player, [Positions]), could we just have the winning player color?
--          the set of positions could be complicated to grab, and I don't think we will even do anything with it
--          its not like we have the cords stored to change anything in our output on a win
data Winner = Winner Player | Tie | Ongoing deriving (Eq, Show, Ord)

--                                       Type Declaration

--Move is the integer value of what column the move is trying to be placed in, should be in range 1-7 always
type Move = Int

-- Game is the current game state, dimensions 7 columns and 6 rows
-- Indexing for colomns and rows starts at 1 and ends at 7 and 6 respectivly
--  i.e.  1  2  3  4  5  6  7
--     1 -  -  -  -  -  -  -
--     2 -  -  -  -  -  -  -
--     3 -  -  -  -  -  -  -
--     4 -  -  -  -  -  -  -
--     5 -  -  -  -  -  -  -
--     6 -  -  -  -  -  -  -    
type Game = (Player, Board)
type Board = [[Position]]

--                                       Sprint 1
-- Helper function that returns the opponent of the player
opponent :: Player -> Player
opponent Red = Yellow
opponent Yellow = Red

-- Make move takes in a game, a move the player is attempting to make, and returns the new game on the other
-- players turn with that player's move if it was valid. currently throws an error if move is out of bounds,
-- but for some reason the error doesn't show until the new board is attempted to be printed
makeMove :: Game -> Move -> Game
makeMove (p, board) move 
  | checkMove board move = 
      let (pre, col:after) = splitAt (move-1) board
          lengthEmpty = length [space | space <- col, space == Empty]
          (empty, _:tokens) = splitAt (lengthEmpty-1) col
          newCol = empty ++ Player p:tokens
      in (opponent p, pre ++ newCol:after)
  | otherwise = error "Invalid Move"

-- Indexing for board positions with this code starts at 1, i.e. 
-- Game is assumed to be the set with indexes 1,2,3,4,5,6,7
-- NOT 0,1,2,3,4,5,6 
checkMove :: Board -> Move -> Bool
checkMove [] _ = error "Column not in board"
checkMove (x:_) 1 = head x == Empty
checkMove (x:xs) move = checkMove xs (move-1)
-- NOTE: checkMove might be unecessary now, we could just generate the set of moves at the beginning of
-- each turn, and check if an attempted move is a member of that set instead of calling this 

validMoves :: Board -> [Move]
validMoves board = aux board 1
    where aux :: Board -> Int -> [Move]
          aux [x] ind = [ind | spaceInCol x]
          aux (x:xs) ind = if spaceInCol x then ind:aux xs (ind+1) else aux xs (ind+1)
--helper for validMoves
spaceInCol :: [Position] -> Bool
spaceInCol (x:_) = x == Empty

-- Checks all posibilities of vertical horizontal and diagonal win states, if any state has a
-- winner it is returned
wonGame :: Board -> Maybe Winner
wonGame board =
    let vertical = [checkFourDown columns | columns <- board]
        horizontal = helperHorizontal board
        diagonalDown = helperDiagonalDown board
        diagonalUp = helperDiagonalUp board
        potentialWinner = (diagonalUp:diagonalDown:horizontal:vertical)
    in if Winner Yellow `elem` potentialWinner
       then Just (Winner Yellow)
       else if Winner Red `elem` potentialWinner
       then Just (Winner Red)
       else fullBoard board

-- Checks if the board is completely filled with pieces
fullBoard :: Board -> Maybe Winner
fullBoard board = if Ongoing `elem` [fullRow row | row <- board] then Nothing else Just Tie

-- Checks if a row is completely full, helper for fullBoard
fullRow :: [Position] -> Winner
fullRow [] = Tie
fullRow (Empty:xs) = Ongoing
fullRow (_:xs) = fullRow xs

-- Checks if there are four in a row vertically
checkFourDown :: [Position] -> Winner
checkFourDown [] = Ongoing
checkFourDown (Player Red:Player Red:Player Red:Player Red:xs) = Winner Red
checkFourDown (Player Yellow:Player Yellow:Player Yellow:Player Yellow:xs) = Winner Yellow
checkFourDown (x:xs) = checkFourDown xs

-- Checks if there are four in a row horizontally, either straight across or diagonal
checkFourAcross :: [Position] -> [Position] -> [Position] -> [Position] -> Winner
checkFourAcross (Player Red:_) (Player Red:_) (Player Red:_) (Player Red:_) = Winner Red
checkFourAcross (Player Yellow:_) (Player Yellow:_) (Player Yellow:_) (Player Yellow:_) = Winner Yellow
checkFourAcross (_:lst1) (_:lst2) (_:lst3) (_:lst4) = checkFourAcross lst1 lst2 lst3 lst4
checkFourAcross _ _ _ _ = Ongoing

-- Helper functions that checks if we have a winner
orW :: Winner -> Winner -> Winner
orW Ongoing w2 = w2
orW w1 w2 = w1

--Sends four horizontal pieces to checkFourAcross function
helperHorizontal :: [[Position]] -> Winner
helperHorizontal (lst1:lst2:lst3:lst4:xs) = orW winner (helperHorizontal (lst2:lst3:lst4:xs))
   where winner = checkFourAcross lst1 lst2 lst3 lst4
helperHorizontal (_:xs) = Ongoing

-- Sends four diagonal pieces starting from top left to bottom right to checkFourAcross function
helperDiagonalDown :: [[Position]] -> Winner
helperDiagonalDown (lst1:lst2:lst3:lst4:xs) = orW winner (helperDiagonalDown (lst2:lst3:lst4:xs))
   where winner = checkFourAcross lst1 (drop 1 lst2) (drop 2 lst3) (drop 3 lst4)
helperDiagonalDown (_:xs) = Ongoing

-- Sends four diagonal pieces starting from bottom left to top right to checkFourAcross function
helperDiagonalUp :: [[Position]] -> Winner
helperDiagonalUp (lst1:lst2:lst3:lst4:xs) = orW winner (helperDiagonalUp (lst2:lst3:lst4:xs))
   where winner = checkFourAcross (drop 3 lst1) (drop 2 lst2) (drop 1 lst3) lst4
helperDiagonalUp (_:xs) = Ongoing

-- Takes a game and converts it to a string, Currently using list comprehension cause
-- NOTE: I got weird errors when trying to pattern match. Im leaving pattern match attempts incase we come back to it
-- Call using the following format to split new lines in output:
-- putStrLn (prettyPrint board)

prettyPrint :: Game -> String
prettyPrint (player, board) = show player ++ "\n" ++ prettyPrintBoard board

prettyPrintBoard :: Board -> String
prettyPrintBoard [[]] = []--error "Game is empty"
prettyPrintBoard board
    | null (head board) = error "You Lost your board"
    | length (head board) > 1 =
        let line = [head col | col <- board]
            remainder = [tail col | col <- board]
        in prettyHelper line ++ "\n" ++ prettyPrintBoard remainder
    | length (head board) == 1 = prettyHelper [head col | col <- board]
    | otherwise = error "negative length list?? somehow??"
-- OLD CODE: prettyPrint [(a:as), (b:bs), (c:cs), (d:ds), (e:es), (f:fs), (g:gs)] =
--   prettyHelper a:b:c:d:e:f:g:"/n" (prettyPrint as:bs:cs:ds:es:fs:gs)-}
-- prettyPrint [a,b,c,d,e,f,g]
--   | length a > 1 = prettyHelper (head a++head b++head c++head d++head e++head f ++ head g) -}

-- Helper function which takes a [Position] and returns it in a printable way
prettyHelper :: [Position] -> String
prettyHelper [] = ""
prettyHelper (x:xs)
    | x == Player Red      = "R " ++ prettyHelper xs
    | x == Player Yellow   = "Y " ++ prettyHelper xs
    | otherwise            = "- " ++ prettyHelper xs

--                                       Sprint 2

-- Takes a game and determines the best outcome for the current player
whoWillWin :: Game -> Winner
whoWillWin (p,b) = 
 case wonGame b of
     Just gameStatus -> gameStatus
     Nothing -> 
       let potentialMoves = validMoves b
           potentialGames = [makeMove (p,b) move | move <- potentialMoves]
           listPotentialGames = map whoWillWin potentialGames
       in if Winner p `elem` listPotentialGames 
          then Winner p 
          else if Tie `elem` listPotentialGames 
          then Tie
          else Winner $ opponent p

-- Takes a game and checks all the possible outcomes of the game and tells the player the best possible move and the outcome it gives
bestMove :: Game -> (Maybe Move, Winner)
bestMove (p,b) = 
  case wonGame b of
    Just gameStatus -> (Nothing, gameStatus)
    Nothing -> 
      let moves = validMoves b
          potentialWinners = [(Just move, whoWillWin (makeMove (p,b) move)) | move <- moves]
      in if any (\(m,w) -> w == Winner p) potentialWinners
         then aux potentialWinners (Winner p)
         else if any (\(m,w) -> w == Tie) potentialWinners
         then aux potentialWinners Tie
         else (Nothing, Winner $ opponent p)
            where aux [] _ = error "something went wrong"
                  aux ((Just move, winner):xs) key = if winner == key then (Just move, winner) else aux xs key

readGame :: String -> Game
readGame input = 
    let 
        columnList = lines input 
        currentPlayer = stringToPlayer (head columnList)
        myBoard = [readHelper c | c <- tail columnList]
    in (currentPlayer, myBoard)
        
stringToPlayer :: String -> Player
stringToPlayer  "R" = Red
stringToPlayer "Y" = Yellow

readHelper :: String -> [Position]
readHelper [] = []
readHelper (s:ss)
    | s == 'R'  = Player Red : readHelper ss
    | s == 'Y'  = Player Yellow : readHelper ss
    | otherwise = Empty : readHelper ss
                
--showGame for Story 13
showGame :: Game -> String
showGame (Red, board) = unlines ("R":[showPosition c | c <- board])
showGame (Yellow, board) = unlines ("Y":[showPosition c | c <- board])

showPosition :: [Position] -> String
showPosition [] = []
showPosition (Empty:xs)         = 'E':showPosition xs
showPosition (Player Red:xs)    = 'R':showPosition xs
showPosition (Player Yellow:xs) = 'Y':showPosition xs

-- NOTE: not sure if we want to move these into testFiles or just get rid of them
blankBoard = replicate 7 (replicate 6 Empty)

sampleBoard = [[Empty,Empty,Empty,Player Red,Player Red,Player Red],
               [Empty,Empty,Empty,Empty,Empty,Player Yellow],
               [Player Red,Player Red,Player Yellow,Player Yellow,Player Red,Player Red],
               [Empty,Empty,Empty,Empty,Player Yellow,Player Yellow],
               [Empty,Empty,Empty,Empty,Empty,Empty],
               [Empty,Player Yellow,Player Yellow,Player Red,Player Red,Player Yellow],
               [Empty,Empty,Empty,Empty,Empty,Player Yellow]]
               --9 red 9 yellow
sampleGameR = (Red, sampleBoard)
sampleGameY = (Yellow, sampleBoard)
               --can be anyones turn, there is a winning move for red and yellow

--                                       Sprint 3

-- data type used for rating the state of a game
type Rating = Int
-- checks the score for the current player
-- if yellow is more likely to win the score is positive, if red is more likely to win it is negative
rateGame :: Game -> Rating
rateGame (player, board) = 
  case wonGame board of
    Just gameStatus -> 
      if      gameStatus == Winner Yellow then 2000
      else if gameStatus == Winner Red    then -2000
      else 0
    Nothing -> 
      let vertical = sum [checkScoreDown columns | columns <- board]
          horizontal = rateGameHorizontal board
          diagonalDown = rateGameDiagonalDown board
          diagonalUp = rateGameDiagonalUp board
      in vertical + horizontal + diagonalDown + diagonalUp

-- checks score for all the columns
checkScoreDown :: [Position] -> Rating
checkScoreDown (x:xs)
  | length (x:xs) >= 4 =
       let four = take 4 (x:xs)
           rateFour = scoreChecker four
       in rateFour + checkScoreDown xs
  | otherwise = 0

-- configures horizontal rows to be passed into checkScoreAcross
rateGameHorizontal :: [[Position]] -> Rating
rateGameHorizontal (lst1:lst2:lst3:lst4:xs) = 
   let rateFour = checkScoreAcross lst1 lst2 lst3 lst4
   in rateFour + rateGameHorizontal (lst2:lst3:lst4:xs)
rateGameHorizontal (_:xs) = 0

-- configures diagonal down rows to be passed into checkScoreAcross
rateGameDiagonalDown :: [[Position]] -> Rating
rateGameDiagonalDown (lst1:lst2:lst3:lst4:xs) = 
   let rateFour = checkScoreAcross lst1 (drop 1 lst2) (drop 2 lst3) (drop 3 lst4)
   in rateFour + rateGameDiagonalDown (lst2:lst3:lst4:xs)
rateGameDiagonalDown (_:xs) = 0

-- configures diagonal up rows to be passed into checkScoreAcross
rateGameDiagonalUp :: [[Position]] -> Rating
rateGameDiagonalUp (lst1:lst2:lst3:lst4:xs) = 
   let rateFour = checkScoreAcross (drop 3 lst1) (drop 2 lst2) (drop 1 lst3) lst4
   in rateFour + rateGameDiagonalUp (lst2:lst3:lst4:xs)
rateGameDiagonalUp (_:xs) = 0

-- calculates score of horizontal and diagonal wins
checkScoreAcross :: [Position] -> [Position] -> [Position] -> [Position] -> Rating
checkScoreAcross [] [] [] [] = 0
checkScoreAcross [] _ _ _ = 0
checkScoreAcross _ [] _ _ = 0
checkScoreAcross _ _ [] _ = 0
checkScoreAcross _ _ _ [] = 0
checkScoreAcross (x1:xs1) (x2:xs2) (x3:xs3) (x4:xs4) = 
   let four = [x1] ++ [x2] ++ [x3] ++ [x4]
       rateFour = scoreChecker four
   in rateFour + checkScoreAcross xs1 xs2 xs3 xs4

-- checks the score of any given set of 4 pieces, positive scores are for player yellow
-- and negative scores are for player red
scoreChecker :: [Position] -> Rating
scoreChecker four =
   if Player Red `elem` four && not (Player Yellow `elem` four)
   then -1
   else if not (Player Red `elem` four) && Player Yellow `elem` four
   then 1
   else 0

-- looks through the game until a certain depth and returns the best rating of the board for
-- for that player and the move the current player should make 
whoMightWin :: Game -> Int -> (Rating, Maybe Move)
whoMightWin (p, b) depth 
  | depth < 0  = error "invalid input"
  | depth == 0 = (rateGame (p,b), Nothing) 
  | otherwise =
    case wonGame b of
      Just gameStatus -> 
        if      gameStatus == Winner Yellow then (2000, Nothing)
        else if gameStatus == Winner Red    then (-2000, Nothing)
        else (0, Nothing)
      Nothing -> 
        let potentialMoves = validMoves b
            potentialGames = [(makeMove (p,b) move, Just move) | move <- potentialMoves]
            ratingsList = map (\(game, move) -> (fst $ whoMightWin game (depth-1), move)) potentialGames
        in if p == Yellow
           then maximum ratingsList --maximumOptim potentialGames depth
           else minimum ratingsList --minimumOptim potentialGames depth
{-
maximumOptim :: [(Game, Maybe Move)] -> Int -> (Rating, Maybe Move)
maximumOptim [] depth = (-2000, Nothing)
maximumOptim ((game, move):xs) depth = 
  let result = whoMightWin game (depth-1)
  in if fst result == 2000 then (fst result, move) else max (fst result, move) (maximumOptim xs depth)

minimumOptim :: [(Game, Maybe Move)] -> Int -> (Rating, Maybe Move)
minimumOptim [] depth = (2000, Nothing)
minimumOptim ((game, move):xs) depth = 
  let result = whoMightWin game (depth-1)
  in if fst result == -2000 then (fst result, move) else min (fst result, move) (minimumOptim xs depth)
-}

sampleBoard2 = [[Empty,Empty,Empty,Empty,Player Red,Player Red],
               [Empty,Empty,Empty,Empty,Empty,Player Yellow],
               [Player Red,Player Red,Player Yellow,Player Yellow,Player Red,Player Red],
               [Empty,Empty,Empty,Empty,Player Yellow,Player Yellow],
               [Empty,Empty,Empty,Empty,Empty,Empty],
               [Empty,Player Yellow,Player Yellow,Player Red,Player Red,Player Yellow],
               [Empty,Empty,Empty,Player Red, Player Red, Player Red]]

sampleGameY2 = (Yellow, sampleBoard2)
sampleGameR2 = (Red, sampleBoard2)
