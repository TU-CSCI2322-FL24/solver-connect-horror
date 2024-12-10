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

--                                       Additional Helper Functions

-- Helper functions that checks if we have a winner
orW :: Winner -> Winner -> Winner
orW Ongoing w2 = w2
orW w1 w2 = w1

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