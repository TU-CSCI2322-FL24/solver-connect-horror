
--Data declaration

--Player is the token color of each player, either Red or Yellow
data Player = Red | Yellow deriving (Eq, Show)

--Position is the value in each spot of the game boared, either a player token, or an empty space
data Position = Player Player | Empty deriving (Eq, Show)

--Winner is either the Player and the set of moves they won with, or a tied board state where every slot is filled
--and there is no winning set of moves
--QUESTION: instead of having a tuple (Player, [Positions]), could we just have the winning player color?
--          the set of positions could be complicated to grab, and I don't think we will even do anything with it
--          its not like we have the cords stored to change anything in our output on a win
data Winner = Tuple (Player, [Position]) | Game deriving (Eq, Show)

--Type declaration
--Move is the integer value of what column the move is trying to be placed in, should be in range 1-7 always
type Move = Int

{- Game is the current game state, dimensions 7 columns and 6 rows
   Indexing for colomns and rows starts at 1 and ends at 7 and 6 respectivly
   i.e.  1  2  3  4  5  6  7
       1 -  -  -  -  -  -  -
       2 -  -  -  -  -  -  -
       3 -  -  -  -  -  -  -
       4 -  -  -  -  -  -  -
       5 -  -  -  -  -  -  -
       6 -  -  -  -  -  -  -        -}
type Game = [[Position]]



{-Indexing for board positions with this code starts at 1, i.e. 
  Game is assumed to be the set with indexes 1,2,3,4,5,6,7
  NOT 0,1,2,3,4,5,6 
  TODO: figure out how to get this to work with 0 indexing
        if we decide thats what we want-}
checkMove :: Game -> Move -> Bool
checkMove [] _ = error "Column not in board"
checkMove (x:_) 1 = Empty `elem` x
checkMove (x:xs) move = checkMove xs (move-1)
