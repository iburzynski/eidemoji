module Main where

import Control.Monad ( void )
import Control.Monad.Except ( runExceptT )
import Control.Monad.Reader ( runReaderT )
import Control.Monad.State.Strict ( StateT(runStateT) )

import Game ( makeBoardMap, playTurn )
import Tables ( difficultyTable, logo )
import Types (Game(..))
import Utils ( getMapVal )

main :: IO ()
main = do
  mapM_ putStrLn logo
  (_, z) <- getMapVal "difficulty" ["normal", "hard", "insane"] difficultyTable
  eb     <- runExceptT $ runReaderT makeBoardMap z
  case eb of
    Left err -> print err
    Right b  -> let g = Game { getStats = (z ^ (2 :: Int), 0)
                             , getBoard = b
                             }
                in void (runReaderT (runStateT playTurn g) z)