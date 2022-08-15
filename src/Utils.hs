module Utils where

import Data.Array.IO ( newListArray, readArray, writeArray, IOArray )
import Data.Bifunctor ( first )
import Data.Char ( toUpper )
import Data.Map.Strict ( Map )
import System.Random.Stateful ( globalStdGen, UniformRange(uniformRM) )
import qualified Data.Map.Strict as M

getMapVal :: String -> [String] -> Map String a -> IO (String, a)
getMapVal p os m = do
  mapM_ putStrLn
    ( concat
      [ "Select a "
      , p
      , ":"
      ] : map fmtOption os
    )
  x <- map toUpper <$> getLine
  let m' = M.fromList . map (first (map toUpper)) . M.toList $ m
  case M.lookup x m' of
    Nothing -> putStrLn "Invalid selection..." >> getMapVal p os m'
    Just y  -> pure (x, y)
  where
    fmtOption :: String -> String
    fmtOption []     = []
    fmtOption (x:xs) = mconcat [ "("
                               , [toUpper x]
                               , ")"
                               , xs ]

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  mapM
    (\i -> do
      j  <- uniformRM (i, n) globalStdGen
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      pure vj
    ) [1..n]
  where
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray = newListArray . (1,)
    n = length xs