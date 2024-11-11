
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

wonGame :: Game -> Winner
wonGame game = 
    let vertical = [checkFourDown columns | columns <- game]
        horizontal = checkFourAcross helper rows
    in if vertical == Yellow || Yellow `elem` horizontal then Yellow else if vertical == Red || Red `elem` horizontal then Red else None

    
checkFourDown :: [Position] -> Winner
checkFourDown [] = None
checkFourDown (Move Red:Move Red:Move:Move Red:Move Red:xs) = Red
checkFourDown (Move Yellow:Move Yellow:Move:Move Yellow:Move Yellow:xs) = Yellow
checkFourDown (x:xs) = checkFourDown xs

checkFourAcross :: Position -> Position -> Position -> Position -> Position -> Position -> Winner
checkFourAcross _ _ (Move Red) (Move Red) (Move Red) (Move Red) = Red
checkFourAcross _ Move Red Move Red Move Red Move Red _ = Red
checkFourAcross Move Red Move Red Move Red Move Red _ _ = Red
checkFourAcross _ _ Move Yellow Move Yellow Move Yellow Move Yellow = Yellow
checkFourAcross _ Move Yellow Move Yellow Move Yellow Move Yellow _ = Yellow
checkFourAcross Move Yellow Move Yellow Move Yellow Move Yellow _ _ = Yellow
checkFourAcross _ _ _ _ _ _ = None

helper :: [Position] -> [Winner]
helper [] = None
helper rows = checkFourAcross (map head rows):helper (map last rows)


    {-
checkFour :: [Position] -> Position -> Int -> Winner
--checkFour (Move Red:Move Red:Move:Move Red:Move)
checkFour xs prev 4 = (prev, xs)
checkFour (x:xs) Empty _ = aux xs x 0
checkFour [] _ _ = game
checkFour (x:xs) prev count = if x == prev then aux xs prev (count+1) else aux xs x 0

checkFourDiagonalDown 
checkFourDiagonalDown xs prev 4 = (prev, xs)
checkFourDiagonalDown (x:xs) Empty _ = aux xs x 0
checkFourDiagonalDown [] _ _ = game
checkFourDiagonalDown (x:xs) prev count = if x == prev then aux xs prev (count+1) else aux xs x 0

{-
horizontalConverter :: [Position] -> [Position]
horizontalConverter [[x]] = 
horizontalConverter rows = [head row | row <- rows]:horizontalConverter [tail row | row <- rows]
-}
-}
