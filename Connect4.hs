Player = Red | Yellow
Position = Move Player | Empty
Game = [[Position]] --[position] is by column
Winner = (Move, [Position]) | Game
Move = Int
