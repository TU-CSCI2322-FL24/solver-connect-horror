module Connect4 where

import Data.Maybe 

--Data declaration

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
type Game = (Player, Board)
type Board = [[Position]]

-- make move takes in a game, a move the player is attempting to make, and returns the new game on the other
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
      
{- takes a game and converts it to a string, Currently using list comprehension cause
   I got weird errors when trying to pattern match. Im leaving pattern match attempts incase we come back to it
   Call using the following format to split new lines in output:
   putStrLn (prettyPrint board)
   -}
prettyPrint :: Board -> String
prettyPrint [[]] = []--error "Game is empty"
prettyPrint board
    | null (head board) = error "You Lost your board"
    | length (head board) > 1 =
        let line = [head col | col <- board]
            remainder = [tail col | col <- board]
        in prettyHelper line ++ "\n" ++ prettyPrint remainder
    | length (head board) == 1 = prettyHelper [head col | col <- board]
    | otherwise = error "negative length list?? somehow??"
{-prettyPrint [(a:as), (b:bs), (c:cs), (d:ds), (e:es), (f:fs), (g:gs)] =
    prettyHelper a:b:c:d:e:f:g:"/n" (prettyPrint as:bs:cs:ds:es:fs:gs)-}
{-prettyPrint [a,b,c,d,e,f,g]
    | length a > 1 = prettyHelper (head a++head b++head c++head d++head e++head f ++ head g) -}


-- Helper function which takes a [Position] and returns it in a printable way
prettyHelper :: [Position] -> String
prettyHelper [] = ""
prettyHelper (x:xs)
    | x == Player Red      = "R " ++ prettyHelper xs
    | x == Player Yellow   = "Y " ++ prettyHelper xs
    | otherwise            = "- " ++ prettyHelper xs

{-Indexing for board positions with this code starts at 1, i.e. 
  Game is assumed to be the set with indexes 1,2,3,4,5,6,7
  NOT 0,1,2,3,4,5,6 -}
checkMove :: Board -> Move -> Bool
checkMove [] _ = error "Column not in board"
checkMove (x:_) 1 = head x == Empty
checkMove (x:xs) move = checkMove xs (move-1)
{-checkMove might be unecessary now, we could just generate the set of moves at the beginning of
    each turn, and check if an attempted move is a member of that set instead of calling this -}


validMoves :: Board -> [Move]
validMoves board = aux board 1
    where aux :: Board -> Int -> [Move]
          aux [x] ind = [ind | spaceInCol x]
          aux (x:xs) ind = if spaceInCol x then ind:aux xs (ind+1) else aux xs (ind+1)
--helper for validMoves
spaceInCol :: [Position] -> Bool
spaceInCol (x:_) = x == Empty

-- runs through all posibilities of vertical horizontal and diagonal win states, if any find a
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

fullBoard :: Board -> Maybe Winner
fullBoard board = if Ongoing `elem` [fullRow row | row <- board] then Nothing else Just Tie

fullRow :: [Position] -> Winner
fullRow [] = Tie
fullRow (Empty:xs) = Ongoing
fullRow (_:xs) = fullRow xs

-- checks if there are four in a row vertically
checkFourDown :: [Position] -> Winner
checkFourDown [] = Ongoing
checkFourDown (Player Red:Player Red:Player Red:Player Red:xs) = Winner Red
checkFourDown (Player Yellow:Player Yellow:Player Yellow:Player Yellow:xs) = Winner Yellow
checkFourDown (x:xs) = checkFourDown xs

-- checks if there are four in a row horizontally, either straight across or diagonal
checkFourAcross :: [Position] -> [Position] -> [Position] -> [Position] -> Winner
checkFourAcross (Player Red:_) (Player Red:_) (Player Red:_) (Player Red:_) = Winner Red
checkFourAcross (Player Yellow:_) (Player Yellow:_) (Player Yellow:_) (Player Yellow:_) = Winner Yellow
checkFourAcross (_:lst1) (_:lst2) (_:lst3) (_:lst4) = checkFourAcross lst1 lst2 lst3 lst4
checkFourAcross _ _ _ _ = Ongoing

-- helper functions that checks if we have a winner
orW :: Winner -> Winner -> Winner
orW Ongoing w2 = w2
orW w1 w2 = w1

-- sends four horizontal pieces to checkFourAcross function
helperHorizontal :: [[Position]] -> Winner
helperHorizontal (lst1:lst2:lst3:lst4:xs) = orW winner (helperHorizontal (lst2:lst3:lst4:xs))
   where winner = checkFourAcross lst1 lst2 lst3 lst4
helperHorizontal (_:xs) = Ongoing

-- sends four diagonal pieces starting from top left to bottom right to checkFourAcross function
helperDiagonalDown :: [[Position]] -> Winner
helperDiagonalDown (lst1:lst2:lst3:lst4:xs) = orW winner (helperDiagonalDown (lst2:lst3:lst4:xs))
   where winner = checkFourAcross lst1 (drop 1 lst2) (drop 2 lst3) (drop 3 lst4)
helperDiagonalDown (_:xs) = Ongoing

-- sends four diagonal pieces starting from bottom left to top right to checkFourAcross function
helperDiagonalUp :: [[Position]] -> Winner
helperDiagonalUp (lst1:lst2:lst3:lst4:xs) = orW winner (helperDiagonalUp (lst2:lst3:lst4:xs))
   where winner = checkFourAcross (drop 3 lst1) (drop 2 lst2) (drop 1 lst3) lst4
helperDiagonalUp (_:xs) = Ongoing

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

readGame :: String -> Game
readGame input = 
    let 
        columnList = lines input 
        currentPlayer = stringToPlayer (head columnList)
        myBoard = [readHelper c | c <- columnList]
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

-- I think we need to change the data type for makeMove because instead of calling an error when a
-- row is full, it should return something else otherwise whoWillWin will crash - it also makes
-- sense to change it for general gameplay because if you make a mistake you dont want the whole
-- game to crash you just want to tell the player they need to try again 
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
                
--showGame for Story 13
showGame :: Game -> String
showGame (Red, board) = unlines ("R":[showPosition c | c <- board])
showGame (Yellow, board) = unlines ("Y":[showPosition c | c <- board])

showPosition :: [Position] -> String
showPosition [] = []
showPosition (Empty:xs)         = 'E':showPosition xs
showPosition (Player Red:xs)    = 'R':showPosition xs
showPosition (Player Yellow:xs) = 'Y':showPosition xs

bestMove :: Game -> Move
bestMove (Red,b) = 
  let moves = validMoves b
      potentialWinners = [whoWillWin (makeMove (Red,b) move) | move <- moves]
      assocList = zip moves potentialWinners
  in if Winner Red `elem` potentialWinners 
     then aux assocList (Winner Red)
     else if Tie `elem` potentialWinners
     then aux assocList Tie
     else head moves
        where aux [] _ = error "something went wrong"
              aux ((move, winner):xs) key = if winner == key then move else aux xs key
bestMove (Yellow,b) = 
  let moves = validMoves b
      potentialWinners = [whoWillWin (makeMove (Yellow,b) move) | move <- moves]
      assocList = zip moves potentialWinners
  in if Winner Yellow `elem` potentialWinners 
     then aux assocList (Winner Yellow)
     else if Tie `elem` potentialWinners
     then aux assocList Tie
     else head moves
        where aux [] _ = error "something went wrong"
              aux ((move, winner):xs) key = if winner == key then move else aux xs key

