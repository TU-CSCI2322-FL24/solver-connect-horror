
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
data Winner = Winner Player | None deriving (Eq, Show)

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
  NOT 0,1,2,3,4,5,6 -}
checkMove :: Game -> Move -> Bool
checkMove [] _ = error "Column not in board"
checkMove (x:_) 1 = head x == Empty
checkMove (x:xs) move = checkMove xs (move-1)
{-checkMove might be unecessary now, we could just generate the set of moves at the beginning of
    each turn, and check if an attempted move is a member of that set instead of calling this -}


validMoves :: Game -> [Move]
validMoves board = aux board 1
    where aux :: Game -> Int -> [Move]
          aux [x] ind = [ind | spaceInCol x]
          aux (x:xs) ind = if spaceInCol x then ind:aux xs (ind+1) else aux xs (ind+1)
--helper for validMoves
spaceInCol :: [Position] -> Bool
spaceInCol (x:_) = x == Empty

type Game = [[Position]]

wonGame :: Game -> Winner
wonGame game =
    let vertical = [checkFourDown columns | columns <- game]
        horizontal = helperHorizontal game
        diagonalDown = helperDiagonalDown game
        diagonalUp = helperDiagonalUp game
    in if Winner Yellow `elem` vertical || Winner Yellow == horizontal || Winner Yellow == diagonalDown || Winner Yellow == diagonalUp
       then Winner Yellow
       else if Winner Red `elem` vertical || Winner Red == horizontal || Winner Red == diagonalDown || Winner Red == diagonalUp
       then Winner Red
       else None


checkFourDown :: [Position] -> Winner
checkFourDown [] = None
checkFourDown (Move Red:Move Red:Move Red:Move Red:xs) = Winner Red
checkFourDown (Move Yellow:Move Yellow:Move Yellow:Move Yellow:xs) = Winner Yellow
checkFourDown (x:xs) = checkFourDown xs

checkFourAcross :: [Position] -> [Position] -> [Position] -> [Position] -> Winner
checkFourAcross (Move Red:_) (Move Red:_) (Move Red:_) (Move Red:_) = Winner Red
checkFourAcross (Move Yellow:_) (Move Yellow:_) (Move Yellow:_) (Move Yellow:_) = Winner Yellow
checkFourAcross (_:lst1) (_:lst2) (_:lst3) (_:lst4) = checkFourAcross lst1 lst2 lst3 lst4
checkFourAcrosss _ _ _ _ = None

orW :: Winner -> Winner -> Winner
orW None w2 = w2
orW w1 w2 = w1

helperHorizontal :: [[Position]] -> Winner
helperHorizontal (lst1:lst2:lst3:lst4:xs) = orW winner (helperHorizontal (lst2:lst3:lst4:xs))
   where winner = checkFourAcross lst1 lst2 lst3 lst4
helperHorizontal (_:xs) = None

helperDiagonalDown :: [[Position]] -> Winner
helperDiagonalDown (lst1:lst2:lst3:lst4:xs) = orW winner (helperDiagonalDown (lst2:lst3:lst4:xs))
   where winner = checkFourAcross lst1 (drop 1 lst2) (drop 2 lst3) (drop 3 lst4)
helperDiagonalDown (_:xs) = None

helperDiagonalUp :: [[Position]] -> Winner
helperDiagonalUp (lst1:lst2:lst3:lst4:xs) = orW winner (helperDiagonalUp (lst2:lst3:lst4:xs))
   where winner = checkFourAcross (drop 3 lst1) (drop 2 lst2) (drop 1 lst3) lst4
helperDiagonalUp (_:xs) = None
