{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Lens hiding (Empty)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Console.ANSI
import System.Exit
import Text.Printf

data Turn = X | O deriving (Eq)

instance Show Turn where
  show X = "X"
  show O = "O"

data Place = Empty | Taken Turn deriving (Eq)

instance Show Place where
  show Empty = " "
  show (Taken turn) = show turn

newtype Board = Board [Place]

instance Show Board where
  show (Board [a, b, c, d, e, f, g, h, i]) =
    show a
      ++ " | "
      ++ show b
      ++ " | "
      ++ show c
      ++ "\n"
      ++ "- - - - -\n"
      ++ show d
      ++ " | "
      ++ show e
      ++ " | "
      ++ show f
      ++ "\n"
      ++ "- - - - -\n"
      ++ show g
      ++ " | "
      ++ show h
      ++ " | "
      ++ show i
  show (Board _) = ""

data S = S
  { turn :: Turn,
    board :: Board,
    win :: Bool
  }
  deriving (Show)

switchTurn :: StateT S IO ()
switchTurn =
  modify
    ( \case
        S {turn = X, board, win} -> S {turn = O, board, win}
        S {turn = O, board, win} -> S {turn = X, board, win}
    )

alterBoard :: Int -> StateT S IO Bool
alterBoard n =
  get >>= \S {board = (Board board), turn, win} -> case board !! n of
    Empty -> (put $ S {board = Board ((element n .~ Taken turn) board), turn, win}) >> return False
    (Taken _) -> return True

playTurn :: StateT S IO ()
playTurn =
  get >>= \S {turn, board, win} ->
    liftIO clear
      >> liftIO (print board)
      >> liftIO (putStrLn "Enter your play (1-9)")
      >> liftIO getLine
      >>= \line ->
        alterBoard ((read line :: Int) - 1)
          >>= \again -> when again playTurn

checkRow :: Place -> Place -> Place -> Bool
checkRow x y z
  | all (== Taken X) [x, y, z] = True
  | all (== Taken O) [x, y, z] = True
  | otherwise = False

checkWin :: StateT S IO ()
checkWin =
  get >>= \S {board = x@(Board [a, b, c, d, e, f, g, h, i]), turn, win} ->
    put
      S
        { board = x,
          turn,
          win =
            checkRow a b c
              || checkRow d e f
              || checkRow g h i
              || checkRow a d g
              || checkRow b e h
              || checkRow c f i
              || checkRow c e g
              || checkRow a e i
        }

game :: StateT S IO ()
game =
  get >>= \S {turn, win} -> do
    if win
      then
        liftIO $
          putStrLn (printf "%s has won!" (show turn))
            >> liftIO exitSuccess
      else switchTurn >> playTurn >> checkWin >> game

clear :: IO ()
clear = clearScreen >> setCursorPosition 0 0

main :: IO ()
main =
  clear
    >> evalStateT
      game
      S
        { turn = O,
          board = Board $ replicate 9 Empty,
          win = False
        }
