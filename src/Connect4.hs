{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromMaybe" #-}
module Connect4 where

import Data.Maybe 

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

-- Make move takes in a game, a move the player is attempting to make, and returns the new game on the other
-- players turn with that player's move if it was valid. currently throws an error if move is out of bounds,
-- but for some reason the error doesn't show until the new board is attempted to be printed
makeMove :: Game -> Move -> Game
makeMove (Red, board) move 
  | checkMove board move = 
      let (pre, col:after) = splitAt (move-1) board
          lengthEmpty = length [space | space <- col, space == Empty]
          (empty, _:tokens) = splitAt (lengthEmpty-1) col
          newCol = empty ++ Player Red:tokens
      in (Yellow, pre ++ newCol:after)
  | otherwise = error "Invalid Move"
makeMove (Yellow, board) move 
  | checkMove board move = 
      let (pre, col:after) = splitAt (move-1) board
          lengthEmpty = length [space | space <- col, space == Empty]
          (empty, _:tokens) = splitAt (lengthEmpty-1) col
          newCol = empty ++ Player Yellow:tokens
      in (Red, pre ++ newCol:after)
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
whoWillWin (p, b) = aux (p,b)
   where
     aux :: Game -> Winner
     aux (Red,b) = 
       let gameStatus = wonGame b
       in if isJust gameStatus then fromJust gameStatus else
             let potentialMoves = validMoves b
                 potentialGames = [makeMove (p,b) move | move <- potentialMoves]
                 listPotentialGames = map aux potentialGames
             in if Winner Red `elem` listPotentialGames 
                then Winner Red 
                else if Tie `elem` listPotentialGames 
                then Tie
                else Winner Yellow
     aux (Yellow,b) = 
       let gameStatus = wonGame b
       in if isJust gameStatus then fromJust gameStatus else
             let potentialMoves = validMoves b
                 potentialGames = [makeMove (p,b) move | move <- potentialMoves]
                 listPotentialGames = map aux potentialGames
             in if Winner Yellow `elem` listPotentialGames 
                then Winner Yellow 
                else if Tie `elem` listPotentialGames 
                then Tie 
                else Winner Red

-- Takes a game and checks all the possible outcomes of the game and tells the player the best possible move and the ourcome it gives
bestMove :: Game -> (Move, Winner)
bestMove (Red,b) = 
  let moves = validMoves b
      potentialWinners = [whoWillWin (makeMove (Red,b) move) | move <- moves]
      assocList = zip moves potentialWinners
  in if Winner Red `elem` potentialWinners 
     then aux assocList (Winner Red)
     else if Tie `elem` potentialWinners
     then aux assocList Tie
     else head assocList
        where aux [] _ = error "something went wrong"
              aux ((move, winner):xs) key = if winner == key then (move, winner) else aux xs key
bestMove (Yellow,b) = 
  let moves = validMoves b
      potentialWinners = [whoWillWin (makeMove (Yellow,b) move) | move <- moves]
      assocList = zip moves potentialWinners
  in if Winner Yellow `elem` potentialWinners 
     then aux assocList (Winner Yellow)
     else if Tie `elem` potentialWinners
     then aux assocList Tie
     else head assocList
        where aux [] _ = error "something went wrong"
              aux ((move, winner):xs) key = if winner == key then (move, winner) else aux xs key

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
-- if current player is more likely to win the score is positive, otherwise it is negative
rateGame :: Game -> Rating
rateGame (Red, board) =
   let vertical = sum [checkScoreDown columns 0 Red | columns <- board]
       horizontal = rateGameHorizontal board 0 Red
       diagonalDown = rateGameDiagonalDown board 0 Red
       diagonalUp = rateGameDiagonalUp board 0 Red
   in vertical + horizontal + diagonalDown + diagonalUp
rateGame (Yellow, board) =
   let vertical = sum [checkScoreDown columns 0 Yellow | columns <- board]
       horizontal = rateGameHorizontal board 0 Yellow
       diagonalDown = rateGameDiagonalDown board 0 Yellow
       diagonalUp = rateGameDiagonalUp board 0 Yellow
   in vertical + horizontal + diagonalDown + diagonalUp

-- checks score for all the columns
checkScoreDown :: [Position] -> Rating -> Player -> Rating
checkScoreDown (x:xs) rating Red
  | length (x:xs) >= 4 =
       let four = take 4 (x:xs)
           add = scoreChecker four rating Red 
       in checkScoreDown xs (rating+add) Red
  | otherwise = rating 
checkScoreDown (x:xs) rating Yellow
  | length (x:xs) >= 4 =
       let four = take 4 (x:xs)
           add = scoreChecker four rating Yellow
       in checkScoreDown xs (rating+add) Yellow
  | otherwise = rating 

-- configures horizontal rows to be passed into checkScoreAcross
rateGameHorizontal :: [[Position]] -> Rating -> Player -> Rating
rateGameHorizontal (lst1:lst2:lst3:lst4:xs) rating Red = 
   let add = checkScoreAcross lst1 lst2 lst3 lst4 rating Red
   in rateGameHorizontal (lst2:lst3:lst4:xs) (rating+add) Red
rateGameHorizontal (lst1:lst2:lst3:lst4:xs) rating Yellow = 
   let add = checkScoreAcross lst1 lst2 lst3 lst4 rating Yellow
   in rateGameHorizontal (lst2:lst3:lst4:xs) (rating+add) Yellow
   
-- configures diagonal down rows to be passed into checkScoreAcross
rateGameHorizontal (_:xs) rating _ = rating

rateGameDiagonalDown :: [[Position]] -> Rating -> Player -> Rating
rateGameDiagonalDown (lst1:lst2:lst3:lst4:xs) rating Red = 
   let add = checkScoreAcross lst1 (drop 1 lst2) (drop 2 lst3) (drop 3 lst4) rating Red
   in rateGameDiagonalDown (lst2:lst3:lst4:xs) (rating+add) Red
rateGameDiagonalDown (lst1:lst2:lst3:lst4:xs) rating Yellow = 
   let add = checkScoreAcross lst1 (drop 1 lst2) (drop 2 lst3) (drop 3 lst4) rating Yellow
   in rateGameDiagonalDown (lst2:lst3:lst4:xs) (rating+add) Yellow
rateGameDiagonalDown (_:xs) rating _ = rating

-- configures diagonal up rows to be passed into checkScoreAcross
rateGameDiagonalUp :: [[Position]] -> Rating -> Player -> Rating
rateGameDiagonalUp (lst1:lst2:lst3:lst4:xs) rating Red = 
   let add = checkScoreAcross (drop 3 lst1) (drop 2 lst2) (drop 1 lst3) lst4 rating Red
   in rateGameDiagonalUp (lst2:lst3:lst4:xs) (rating+add) Red
rateGameDiagonalUp (lst1:lst2:lst3:lst4:xs) rating Yellow = 
   let add = checkScoreAcross (drop 3 lst1) (drop 2 lst2) (drop 1 lst3) lst4 rating Yellow
   in rateGameDiagonalUp (lst2:lst3:lst4:xs) (rating+add) Yellow
rateGameDiagonalUp (_:xs) rating _ = rating

-- calculates score of horizontal and diagonal wins
checkScoreAcross :: [Position] -> [Position] -> [Position] -> [Position] -> Rating -> Player -> Rating
checkScoreAcross [] [] [] [] rating _ = rating
checkScoreAcross [] _ _ _ rating _ = rating
checkScoreAcross _ [] _ _ rating _ = rating
checkScoreAcross _ _ [] _ rating _ = rating
checkScoreAcross _ _ _ [] rating _ = rating
checkScoreAcross (x1:xs1) (x2:xs2) (x3:xs3) (x4:xs4) rating Red = 
   let combined = [x1] ++ [x2] ++ [x3] ++ [x4]
       add = scoreChecker combined rating Red
   in checkScoreAcross xs1 xs2 xs3 xs4 (rating+add) Red
checkScoreAcross (x1:xs1) (x2:xs2) (x3:xs3) (x4:xs4) rating Yellow = 
   let combined = [x1] ++ [x2] ++ [x3] ++ [x4]
       add = scoreChecker combined rating Yellow
   in checkScoreAcross xs1 xs2 xs3 xs4 (rating+add) Yellow

-- checks the score of any given set of 4 pieces
scoreChecker :: [Position] -> Rating -> Player -> Rating
scoreChecker four rating Red =
   if Player Red `elem` four && not (Player Yellow `elem` four)
   then 1
   else if not (Player Red `elem` four) && Player Yellow `elem` four
   then -1
   else 0
scoreChecker four rating Yellow = 
   if Player Red `elem` four && not (Player Yellow `elem` four)
   then -1
   else if not (Player Red `elem` four) && Player Yellow `elem` four
   then 1
   else 0
