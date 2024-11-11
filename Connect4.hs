
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

PrettyPrint :: Game -> String
PrettyPrint [] = error "Game is empty"
PrettyPrint [(a:[]), (b:[]), (c:[]), (d:[]), (e:[]), (f:[]), (g:[])] =
    PrettyHelper a:b:c:d:e:f:g 
PrettyPrint [(a:as), (b:bs), (c:cs), (d:ds), (e:es), (f:fs), (g:gs)] =
    PrettyHelper a:b:c:d:e:f:g:"/n" (PrettyPrint as:bs:cs:ds:es:fs:gs)

-- Helper function which takes a [Position] and returns it in a printable way
PrettyHelper :: [Position] -> String
PrettyHelper (x:[]):
    | x == Red      = "R"
    | x == Yellow   = "Y"
    | otherwise     = "-"
PrettyHelper (x:xs):
    | x == Red      = "R" : PrettyHelper xs
    | x == Yellow   = "Y" : PrettyHelper xs
    | otherwise     = "-" : PrettyHelper xs