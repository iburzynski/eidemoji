module Types where

import Control.Exception ( Exception )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.State.Strict ( StateT )
import Data.Aeson ( eitherDecode, (.:), withObject, FromJSON(parseJSON), ToJSON )
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Char ( chr )
import Data.Map.Strict ( Map )
import GHC.Generics ( Generic )
import Numeric ( readHex )

data SetupException =
    APIException
  | ParseException
  | SizeException
  deriving Show

instance Exception SetupException where

data Emoji =
  Emoji { getEmoji         :: String
        , getEmojiSubgroup :: String
        }
  deriving (Show, Read, Generic)

instance FromJSON Emoji where
  parseJSON = withObject "Emoji" $ \obj -> do
    cp <- obj .: "codePoint"
    sg <- obj .: "subGroup"
    -- convert unicode codepoint to string
    let e = pure . chr . fst . head $ readHex cp
    return $ Emoji { getEmoji = e
                   , getEmojiSubgroup = sg
                   }

instance ToJSON Emoji

data Card       = Card { getImage :: String
                       , isRevealed :: Bool
                       } deriving Eq

data Game a b   = Game { getStats :: a
                       , getBoard :: b
                       }

instance Functor (Game a) where
  fmap f (Game t b) = Game t $ f b

instance Bifunctor Game where
  bimap f g (Game t b) = Game (f t) $ g b

data GameStatus = Playing | Won | Lost deriving Eq

type BoardMap  = Map String Card
type Position  = String
type Score     = Float
type Size      = Int
type Stats     = (Turns, Score)
type Subgroups = [String]
type Turns     = Int
type GameState = StateT (Game Stats BoardMap) (ReaderT Size IO)