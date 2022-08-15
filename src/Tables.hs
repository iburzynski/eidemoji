module Tables where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

difficultyTable :: Map String Int
difficultyTable = M.fromList [
    ("N", 4)
  , ("H", 6)
  , ("I", 8)
  ]

catTable :: Map String [String]
catTable = M.fromList [
    ("A", ["animal-amphibian", "animal-bug", "animal-mammal", "animal-marine", "animal-reptile"])
  , ("F", ["food-fruit", "food-prepared", "food-vegetable"])
  ]

logo :: [String]
logo = [
    " .----------------.  .----------------.  .----------------.  .----------------. "
  , "| .--------------. || .--------------. || .--------------. || .--------------. |"
  , "| |  _________   | || |     _____    | || |  ________    | || |  _________   | |"
  , "| | |_   ___  |  | || |    |_   _|   | || | |_   ___ `.  | || | |_   ___  |  | |"
  , "| |   | |_  \\_|  | || |      | |     | || |   | |   `. \\ | || |   | |_  \\_|  | |"
  , "| |   |  _|  _   | || |      | |     | || |   | |    | | | || |   |  _|  _   | |"
  , "| |  _| |___/ |  | || |     _| |_    | || |  _| |___.' / | || |  _| |___/ |  | |"
  , "| | |_________|  | || |    |_____|   | || | |________.'  | || | |_________|  | |"
  , "| |              | || |              | || |              | || |              | |"
  , "| '--------------' || '--------------' || '--------------' || '--------------' |"
  , " '----------------'  '----------------'  '----------------'  '----------------' "
  , " .----------------.  .----------------.  .----------------.  .----------------. "
  , "| .--------------. || .--------------. || .--------------. || .--------------. |"
  , "| | ____    ____ | || |     ____     | || |     _____    | || |     _____    | |"
  , "| ||_   \\  /   _|| || |   .'    `.   | || |    |_   _|   | || |    |_   _|   | |"
  , "| |  |   \\/   |  | || |  /  .--.  \\  | || |      | |     | || |      | |     | |"
  , "| |  | |\\  /| |  | || |  | |    | |  | || |   _  | |     | || |      | |     | |"
  , "| | _| |_\\/_| |_ | || |  \\  `--'  /  | || |  | |_' |     | || |     _| |_    | |"
  , "| ||_____||_____|| || |   `.____.'   | || |  `.___.'     | || |    |_____|   | |"
  , "| |              | || |              | || |              | || |              | |"
  , "| '------------' || '--------------' || '--------------' || '--------------' |"
  , " '----------------'  '----------------'  '----------------'  '----------------' "
  ]