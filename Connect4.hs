
--Data declaration
data Player = Red | Yellow deriving (Eq, Show)
data Position = Move Player | Empty deriving (Eq, Show)
data Winner = Tuple (Player, [Position]) | Game deriving (Eq, Show)

--Type declaration
type Move = Int
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
