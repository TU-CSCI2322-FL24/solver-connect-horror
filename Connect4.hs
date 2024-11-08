
--Data declaration
data Player = Red | Yellow
data Position = Move Player | Empty
data Winner = Tuple (Player, [Position]) | Game

--Type declaration
type Move = Int
type Game = [[Position]]

wonGame :: Game -> Winner
wonGame game = [aux columns (head columns) 0 | columns <- game]
    where aux xs prev 4 = (prev, xs)
          aux [] _ _ = game
          aux (x:xs) prev count = if x == prev then aux xs prev (count+1) else aux xs x 0