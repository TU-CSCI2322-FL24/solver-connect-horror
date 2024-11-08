
--Data declaration
data Player = Red | Yellow deriving (Eq, Show)
data Position = Move Player | Empty deriving (Eq, Show)
data Winner = Tuple (Player, [Position]) | Game deriving (Eq, Show)

--Type declaration
type Move = Int
type Game = [[Position]]
