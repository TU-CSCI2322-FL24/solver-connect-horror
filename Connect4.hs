
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
