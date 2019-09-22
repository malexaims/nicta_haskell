{-# LANGUAGE FlexibleContexts #-}

module Projects.TicTacToe.TicTacToe where

import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Text.Read as R
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Debug.Trace

data Symbol = X | O deriving (Eq, Show)
type Space = Either Int Symbol
type Board = [[Space]]
data GState = Game { board :: Board, currPlayer :: Symbol}

showSpace :: Space -> String
showSpace (Left i) = show i
showSpace (Right s) = show s

showGameState :: GState -> IO ()
showGameState = putStrLn
              . unlines
              . map (concat . intersperse "|" . map showSpace)
              . board

startingState :: GState
startingState = Game (map (map Left) [[1..3],[4..6],[7..9]]) X

winCondition :: GState -> Bool
winCondition g = any complete (diags board' ++ board' ++ cols board')
 where diags [[a,_,c],
              [_,b1,_],
              [a2,_,c2]] = [[a,b1,c2], [c,b1,a2]]
       cols = transpose
       board' = board g
       complete [x,y,z] = all ((==) x) [y,z]

isDraw :: GState -> Bool
isDraw g = (not $ winCondition g) && (null $ possibleMoves g)

possibleMoves :: GState -> [Int]
possibleMoves g = lefts . concat $ board g

possibleBoards :: GState -> [GState] --All possible board outcomes of possible moves
possibleBoards g = catMaybes $ map (\i -> makeMove i g) (possibleMoves g)

makeMove :: Int -> GState -> Maybe GState --  Nothing on illegal move
makeMove i g = if (i `elem` possibleMoves g)
                then Just g'
               else Nothing
    where g' = Game (map (map opposite) $ board g) (currPlayer g)
          opposite (Left i') | i' == i = Right $ currPlayer g
          opposite i' = i'

makeMove' :: Int -> GState -> GState
makeMove' i g = g'
    where g' = Game (map (map opposite) $ board g) (currPlayer g)
          opposite (Left i') | i' == i = Right $ currPlayer g
          opposite i' = i'

main :: IO ()
main = do
  putStrLn "Starting Game, Player X goes first."
  putStrLn "Enter 'q' at any time to quit."
  putStrLn "Enter 'c1' to continue as two player"
  putStrLn "Enter 'c2' to continue as Player X against an AI Player O"
  entry <- getLine
  case entry of
    "q" -> return ()
    "c1" -> gameLoop startingState
    "c2" -> gameLoop2 startingState
    otherwise -> putStrLn "Invalid input." >> main

-- 2 Player
gameLoop :: GState -> IO ()
gameLoop g = do
  showGameState g
  putStrLn $ "Player " ++ (show $ currPlayer g) ++ " enter a move."
  entry <- getLine
  if entry == "q"
    then return ()
  else
    case R.readMaybe entry of
      Nothing -> putStrLn "Invalid input. Try again." >> gameLoop g
      Just entry' ->
        case makeMove entry' g of
          Nothing -> putStrLn "Invalid input. Try again." >> gameLoop g
          Just g' -> if isDraw g'
                       then putStrLn "Draw."
                     else if winCondition g'
                       then putStrLn ("Player " ++ (show $ currPlayer g) ++ " wins!!")
                     else gameLoop (swapPlayer g')

-- 1 Player against the min-max AI
-- Player X is the default IO player, and O is the AI
gameLoop2 :: GState -> IO ()
gameLoop2 g@(Game b p) = case p of
  O -> do
       showGameState g
       let g' = makeMove' (minMax O g) g
           in
             if isDraw g'
               then putStrLn "Draw."
             else if winCondition g'
               then putStrLn ("Player " ++ (show $ currPlayer g') ++ " wins!!")
             else gameLoop2 (swapPlayer g')
  X ->  do
    showGameState g
    putStrLn $ "Player " ++ (show $ currPlayer g) ++ " enter a move."
    entry <- getLine
    if entry == "q"
     then return ()
    else
      let g' = makeMove' (read entry) g
      in
        if isDraw g'
          then putStrLn "Draw."
        else if winCondition g
          then putStrLn ("Player " ++ (show $ currPlayer g) ++ " wins!!")
        else gameLoop2 (swapPlayer g')

readEntry :: MaybeT IO String
readEntry = liftIO getLine >>= \e ->
              case (R.readMaybe e) of
                  Nothing -> return "Invalid Input"
                  Just e' -> return e'

minMax :: Symbol -> --Player to opimize for
          GState ->
          Int -- Best Move
minMax s g = snd
           $ M.findMax
           $ M.fromList
           $ map play (possibleMoves g)
           where play m = (minPlay s $ makeMove' m g, m)

minPlay :: Symbol -> GState -> Int
minPlay s g  | winCondition g = score s g
             | isDraw g       = 0
             | otherwise = minimum
                         $ map play (possibleMoves g)
                           where play m = maxPlay s $ makeMove' m (swapPlayer g)

maxPlay :: Symbol -> GState -> Int
maxPlay s g | winCondition g = score s g
            | isDraw g       = 0
            | otherwise = maximum
                        $ map play (possibleMoves g)
                          where play m = minPlay s $ makeMove' m (swapPlayer g)


score :: Symbol -> GState -> Int
score X g@(Game b p) | p == X = 10 + length (possibleMoves g)
                     | otherwise = -10 - length (possibleMoves g)
score O g@(Game b p) | p == O = 10 + length (possibleMoves g)
                     | otherwise = -10 - length (possibleMoves g)


flipP :: Symbol -> Symbol
flipP X = O
flipP O = X

swapPlayer :: GState -> GState
swapPlayer (Game b p) = Game b (flipP p)


------------------------------------------------------------------------------
winTest :: GState
winTest = Game (map (map Right) [[X,X,X],
                                 [X,X,X],
                                 [X,X,X]]) X

drawTest :: GState
drawTest = Game (map (map Right) [[X,O,X],
                                  [O,O,X],
                                  [X,X,O]]) X

minMaxTest1 :: IO ()
minMaxTest1 = putStrLn $ show $ minMax X g1
   where g1 = Game ([[Right X, Left 2, Right X],
                     [Right O, Right O, Right X],
                     [Right X, Right X, Right O]]) X

minMaxTest2 :: IO ()
minMaxTest2 = putStrLn $ show $ minMax X g1
  where g1 = Game ([[Left 1, Left 2, Left 3],
                    [Right O, Right O, Right X],
                    [Right X, Right X, Left 9]]) X

minMaxTest3 :: IO ()
minMaxTest3 = do
  let g1 = Game ([[Right X, Right X, Left 3],
                  [Right O, Right O, Left 6],
                  [Right X, Right X, Left 9]]) O
  putStrLn $ show $ possibleMoves g1
  putStrLn $ show $ minMax O g1

minMaxTest4 :: IO ()
minMaxTest4 = putStrLn $ show $ minMax O g1
   where g1 = Game ([[Right O, Right O, Left 3],
                     [Left 4, Left 4, Left 6],
                     [Right X, Left 8, Left 9]]) O

minMaxTest5 :: IO ()
minMaxTest5 = putStrLn $ show $ minMax X g1
  where g1 = Game ([[Right X, Right X, Left 3],
                    [Left 4, Right O, Left 6],
                    [Right O, Left 8, Left 9]]) X
