module Game where

import Control.Monad.Except ( throwError, MonadError )
import Control.Monad.Reader ( MonadIO(liftIO), MonadReader(ask) )
import Control.Monad.State.Strict ( MonadState(get), modify )
import Data.Bifunctor ( first )
import Data.Char ( chr )
import Data.List ( intercalate, nub )
import Data.List.Split ( chunksOf )
import Numeric ( readHex )
import System.Console.ANSI ( clearScreen )
import qualified Data.Map.Strict as M

import Emojis ( filterEmojis, getEmojis )
import Tables ( catTable )
import Types
import Utils ( getMapVal, shuffle )

makeBoardMap :: (MonadReader Int m, MonadError SetupException m, MonadIO m) => m BoardMap
makeBoardMap = do
  -- get emoji category from user
  (_, cat) <- liftIO $ getMapVal "category" ["animals", "food"] catTable
  -- retrieve all emoji data, filter by selected category, remove duplicates and shuffle
  es       <- getEmojis >>= liftIO . shuffle . nub . filterEmojis cat
  s        <- ask
  -- make sure total cards is divisible by two (though odd scenario should be unreachable...)
  if even s
    then liftIO $ M.fromList . zip (makeKeys s) <$> shuffle (makeCards s es)
    else throwError SizeException
  where
    -- make a list of keys based on board size in format "<row letter><col number>" (i.e. "A0")
    makeKeys s     = [ r : show c  | r <- map (chr . (+ 65)) [ 0 .. s - 1 ], c <- [ 0 .. s - 1 ] ]
    -- take emojis equal in length to half the total board area, duplicate each and render to cards
    makeCards s es = [ makeCard e' | e <- take (s ^ 2 `div` 2) es, e' <- [ e, e ] ]
    -- render an emoji into a game card
    makeCard e     = Card { getImage = e, isRevealed = False }

playTurn :: GameState ()
playTurn = do
  g <- get
  case getGameStatus g of
    Lost    -> liftIO $ putStrLn "Game over!"
    Won     -> liftIO . printFinalScore . fst $ getStats g
    Playing -> do
      m1 <- move
      m2 <- move
      endTurn m1 m2
      playTurn
  where
    printFinalScore :: Turns -> IO ()
    printFinalScore turns = putStrLn . concat $
        [ "You win!\n"
        , "Turns: "
        , show turns
        , " | Score: 100%"
        ]
    getGameStatus (Game (t, _) b)
      | t < 1                               = Lost
      | all (isRevealed . snd) $ M.toList b = Won
      | otherwise                           = Playing

move :: GameState Position
move = do
  printState
  g <- get
  (p, c) <- liftIO $ getMapVal "card" [] (getBoard g)
  if not . isRevealed $ c
    then flipCard p
    else liftIO (putStrLn "Invalid move!") >> move

endTurn :: Position -> Position -> GameState ()
endTurn m1 m2 = do
  decTurns
  printState
  g <- get
  let b = getBoard g
      [e1, e2] = flip M.lookup b <$> [m1, m2]
  if e1 == e2
    -- if card emojis match, leave board state unchanged and increment score
    then incScore
    -- if not, flip the cards back over
    else do
      _ <- liftIO (putStrLn "Press ENTER to continue" >> getChar)
      _ <- flipCard m1
      _ <- flipCard m2
      pure ()
  where
    incScore = modify . first . fmap  $ (+ 1)
    decTurns = modify . first . first $ subtract 1

flipCard :: Position -> GameState Position
flipCard p = do
  modify . fmap $ M.adjust (\c -> c { isRevealed = not $ isRevealed c }) p
  pure p

printState :: GameState ()
printState = do
--  liftIO clearFromCursorToScreenBeginning
  liftIO clearScreen
  s <- ask
  g <- get
  let
    rs = chunksOf s . map snd . M.toList . getBoard $ g
    header = concat
      [ "     "
      , intercalate "    " (map show [0 .. s - 1])
      , "  \n  +"
      , concat (replicate s "----+")
      , "\n"
      ]
    showRow r = concatMap showCard r ++ "\n  +" ++ concat (replicate (length r) "----+")
    showCard c =
      concat [ " "
             , if isRevealed c then getImage c else "❔"
             , " |"
             ]
    rows = zipWith (++)
      (map (\x -> [ toEnum $ x + 65, ' ', '|' ]) [ 0 .. s - 1 ]) (map (\r -> showRow r ++ "\n") rs)
    (turns, score) = getStats g
  mapM_ (liftIO . putStrLn . concat)
    [ [ "Turns: "
      , show turns
      , " | Score: "
      , show . (* 100) $ (score / realToFrac (s ^ 2 `div` 2))
      , "%\n"
      ]
    , header : rows
    ]
