module Solver where

import Connect4_n

-- Takes a game and determines the best outcome for the current player
whoWillWin :: Game -> Winner
whoWillWin (p,b) = 
 case wonGame b of
     Just gameStatus -> gameStatus
     Nothing -> 
       let potentialMoves = validMoves b
           potentialGames = [makeMove (p,b) move | move <- potentialMoves]
           listPotentialGames = map whoWillWin potentialGames
       in if Winner p `elem` listPotentialGames 
          then Winner p 
          else if Tie `elem` listPotentialGames 
          then Tie
          else Winner $ opponent p

-- Takes a game and checks all the possible outcomes of the game and tells the player the best possible move and the outcome it gives
bestMove :: Game -> (Maybe Move, Winner)
bestMove (p,b) = 
  case wonGame b of
    Just gameStatus -> (Nothing, gameStatus)
    Nothing -> 
      let moves = validMoves b
          potentialWinners = [(whoWillWin (makeMove (p,b) move), move) | move <- moves]
      in case (lookup (Winner p) potentialWinners, lookup Tie potentialWinners, potentialWinners) of
              (Just m, _, _) -> (Just m, Winner p)
              (Nothing, Just m, _) -> (Just m, Tie)
              (Nothing, Nothing, (loss, move):_) -> (Just move, loss)

-- data type used for rating the state of a game
type Rating = Int

-- checks the score for the current player
-- if yellow is more likely to win the score is positive, if red is more likely to win it is negative
rateGame :: Game -> Rating
rateGame (player, board) = 
  case wonGame board of
       Just (Winner Yellow) -> 2000 
       Just (Winner Red)    -> -2000
       Just Tie             -> 0 
       Nothing -> 
        let vertical = sum [checkScoreDown columns | columns <- board]
            horizontal = rateGameHorizontal board
            diagonalDown = rateGameDiagonalDown board
            diagonalUp = rateGameDiagonalUp board
        in vertical + horizontal + diagonalDown + diagonalUp

-- checks score for all the columns
checkScoreDown :: [Position] -> Rating
checkScoreDown (x:xs)
  | length (x:xs) >= 4 =
       let four = take 4 (x:xs)
           rateFour = scoreChecker four
       in rateFour + checkScoreDown xs
  | otherwise = 0

-- configures horizontal rows to be passed into checkScoreAcross
rateGameHorizontal :: [[Position]] -> Rating
rateGameHorizontal (lst1:lst2:lst3:lst4:xs) = 
   let rateFour = checkScoreAcross lst1 lst2 lst3 lst4
   in rateFour + rateGameHorizontal (lst2:lst3:lst4:xs)
rateGameHorizontal (_:xs) = 0

-- configures diagonal down rows to be passed into checkScoreAcross
rateGameDiagonalDown :: [[Position]] -> Rating
rateGameDiagonalDown (lst1:lst2:lst3:lst4:xs) = 
   let rateFour = checkScoreAcross lst1 (drop 1 lst2) (drop 2 lst3) (drop 3 lst4)
   in rateFour + rateGameDiagonalDown (lst2:lst3:lst4:xs)
rateGameDiagonalDown (_:xs) = 0

-- configures diagonal up rows to be passed into checkScoreAcross
rateGameDiagonalUp :: [[Position]] -> Rating
rateGameDiagonalUp (lst1:lst2:lst3:lst4:xs) = 
   let rateFour = checkScoreAcross (drop 3 lst1) (drop 2 lst2) (drop 1 lst3) lst4
   in rateFour + rateGameDiagonalUp (lst2:lst3:lst4:xs)
rateGameDiagonalUp (_:xs) = 0

-- calculates score of horizontal and diagonal wins
checkScoreAcross :: [Position] -> [Position] -> [Position] -> [Position] -> Rating
checkScoreAcross [] [] [] [] = 0
checkScoreAcross [] _ _ _ = 0
checkScoreAcross _ [] _ _ = 0
checkScoreAcross _ _ [] _ = 0
checkScoreAcross _ _ _ [] = 0
checkScoreAcross (x1:xs1) (x2:xs2) (x3:xs3) (x4:xs4) = 
   let four = [x1] ++ [x2] ++ [x3] ++ [x4]
       rateFour = scoreChecker four
   in rateFour + checkScoreAcross xs1 xs2 xs3 xs4

-- checks the score of any given set of 4 pieces, positive scores are for player yellow
-- and negative scores are for player red
scoreChecker :: [Position] -> Rating
scoreChecker four =
   if Player Red `elem` four && not (Player Yellow `elem` four)
   then -1
   else if not (Player Red `elem` four) && Player Yellow `elem` four
   then 1
   else 0

-- looks through the game until a certain depth and returns the best rating of the board for
-- for that player and the move the current player should make 
whoMightWin :: Game -> Int -> (Rating, Maybe Move)
whoMightWin (p, b) depth 
  | depth < 0  = error "invalid input"
  | depth == 0 = (rateGame (p,b), Nothing) 
  | otherwise =
    case wonGame b of
      Just (Winner Yellow) -> (2000, Nothing) 
      Just (Winner Red)    -> (-2000, Nothing) 
      Just Tie             -> (0, Nothing) 
      Nothing -> 
        let potentialMoves = validMoves b
            potentialGames = [(makeMove (p,b) move, Just move) | move <- potentialMoves]
        in if p == Yellow
           then maximumOptim potentialGames depth
           else minimumOptim potentialGames depth

  
-- helper function for optimizing whoMightWin when it is yellows turn
maximumOptim :: [(Game, Maybe Move)] -> Int -> (Rating, Maybe Move)
maximumOptim [(game, move)] depth = (rateGame game, move)
maximumOptim ((game, move):xs) depth = 
  let result = whoMightWin game (depth-1)
  in if fst result == 2000 then (fst result, move) else max (fst result, move) (maximumOptim xs depth)

-- helper function for optimizing whoMightWin when it is reds turn
minimumOptim :: [(Game, Maybe Move)] -> Int -> (Rating, Maybe Move)
minimumOptim [(game, move)] depth = (rateGame game, move)
minimumOptim ((game, move):xs) depth = 
  let result = whoMightWin game (depth-1)
  in if fst result == -2000 then (fst result, move) else min (fst result, move) (minimumOptim xs depth)