module ReadWrite where

import Connect4_n

-- Takes a game and converts it to a string, Currently using list comprehension cause
-- NOTE: I got weird errors when trying to pattern match. Im leaving pattern match attempts incase we come back to it
-- Call using the following format to split new lines in output:
-- putStrLn (prettyPrint board)
prettyPrint :: Game -> String
prettyPrint (player, board) = show player ++ "\n" ++ prettyPrintBoard board

prettyPrintBoard :: Board -> String
prettyPrintBoard [[]] = []--error "Game is empty"
prettyPrintBoard board
    | null (head board) = error "You Lost your board"
    | length (head board) > 1 =
        let line = [head col | col <- board]
            remainder = [tail col | col <- board]
        in prettyHelper line ++ "\n" ++ prettyPrintBoard remainder
    | length (head board) == 1 = prettyHelper [head col | col <- board]
    | otherwise = error "negative length list?? somehow??"
-- OLD CODE: prettyPrint [(a:as), (b:bs), (c:cs), (d:ds), (e:es), (f:fs), (g:gs)] =
--   prettyHelper a:b:c:d:e:f:g:"/n" (prettyPrint as:bs:cs:ds:es:fs:gs)-}
-- prettyPrint [a,b,c,d,e,f,g]
--   | length a > 1 = prettyHelper (head a++head b++head c++head d++head e++head f ++ head g) -}

-- Helper function which takes a [Position] and returns it in a printable way
prettyHelper :: [Position] -> String
prettyHelper [] = ""
prettyHelper (x:xs)
    | x == Player Red      = "R " ++ prettyHelper xs
    | x == Player Yellow   = "Y " ++ prettyHelper xs
    | otherwise            = "- " ++ prettyHelper xs

-- takes a string with game data and returns it as a game
readGame :: String -> Game
readGame input = 
    let 
        columnList = lines input 
        currentPlayer = stringToPlayer (head columnList)
        myBoard = [readHelper c | c <- tail columnList]
    in (currentPlayer, myBoard)

-- helper function that converts a string to a player 
stringToPlayer :: String -> Player
stringToPlayer  "R" = Red
stringToPlayer "Y" = Yellow

-- takes a string a returns a column of a board
readHelper :: String -> [Position]
readHelper [] = []
readHelper (s:ss)
    | s == 'R'  = Player Red : readHelper ss
    | s == 'Y'  = Player Yellow : readHelper ss
    | otherwise = Empty : readHelper ss
                
-- takes a game and returns it as a string
showGame :: Game -> String
showGame (Red, board) = unlines ("R":[showPosition c | c <- board])
showGame (Yellow, board) = unlines ("Y":[showPosition c | c <- board])

-- takes one column of a board and returns it as a string
showPosition :: [Position] -> String
showPosition [] = []
showPosition (Empty:xs)         = 'E':showPosition xs
showPosition (Player Red:xs)    = 'R':showPosition xs
showPosition (Player Yellow:xs) = 'Y':showPosition xs