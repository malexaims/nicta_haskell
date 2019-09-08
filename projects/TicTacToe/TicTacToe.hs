{-# LANGUAGE FlexibleContexts #-}

module Projects.TicTacToe.TicTacToe where

import Data.List
import Data.Either
import qualified Text.Read as R
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

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

makeMove :: Int -> GState -> Maybe GState --  Nothing on illegal move
makeMove i g = if (i `elem` possibleMoves g)
                then Just g'
               else Nothing
    where g' = Game (map (map opposite) $ board g) (p' $ currPlayer g)
          p' X = O
          p' O = X
          opposite (Left i') | i' == i = Right $ currPlayer g
          opposite i' = i'

main :: IO ()
main = do
  putStrLn "Starting Game, Player X goes first."
  putStrLn "Enter 'q' at any time to quit."
  putStrLn "Game only works for two players (no AI) currently."
  putStrLn "Enter 'c' to continue"
  entry <- getLine
  case entry of
    "q" -> return ()
    "c" -> gameLoop startingState
    otherwise -> putStrLn "Invalid input." >> main

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
                     else gameLoop g'


------------------------------------------------------------------------------
winTest :: GState
winTest = Game (map (map Right) [[X,X,X], [X,X,X], [X,X,X]]) X

drawTest :: GState
drawTest = Game (map (map Right) [[X,O,X],
                                  [O,O,X],
                                  [X,X,O]]) X
