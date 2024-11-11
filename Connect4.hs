
--Data declaration
data Player = Red | Yellow
data Position = Move Player | Empty
data Winner = Tuple (Player, [Position]) | Game

--Type declaration
type Move = Int
type Game = [[Position]]

wonGame :: Game -> Winner
wonGame game = 
    let vertical = [checkFourDown columns | columns <- game]
        horizontal = checkFourAcross helper rows

    
checkFourDown :: [Position] -> Winner
checkFourDown [] = None
checkFourDown (Move Red:Move Red:Move:Move Red:Move Red:xs) = Red
checkFourDown (Move Yellow:Move Yellow:Move:Move Yellow:Move Yellow:xs) = Yellow
checkFourDown (x:xs) = checkFourDown xs

checkFourAcross :: Position -> Position -> Position -> Position -> Position -> Position -> Winner
checkFourDown _ _ Move Red Move Red Move Move Red Move Red = Red
checkFourDown _ Move Red Move Red Move Move Red Move Red _ = Red
checkFourDown Move Red Move Red Move Move Red Move Red _ _ = Red
checkFourDown _ _ Move Yellow Move Yellow Move Move Yellow Move Yellow = Yellow
checkFourDown _ Move Yellow Move Yellow Move Move Yellow Move Yellow _ = Yellow
checkFourDown Move Yellow Move Yellow Move Move Yellow Move Yellow _ _ = Yellow
checkFourDown _ _ _ _ = None

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