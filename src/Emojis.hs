{-# LANGUAGE FlexibleContexts #-}
module Emojis where

import Control.Monad.Except ( liftIO, throwError, MonadIO, MonadError )
import Data.Aeson ( eitherDecode )
import Data.Bifunctor ( first )
import Data.String ( IsString(fromString) )
import Network.HTTP.Simple ( Request, Response, getResponseBody, httpJSONEither, httpLBS )
import System.Directory ( doesFileExist )
import Text.Read (readEither)

import Types

txtPath = "./assets/emojis.txt"
apiHost = "https://emoji-api.com"
apiPath = "/emojis"
myToken = "314117dc6f475e489b0a1ee486272ac0c0886f6e"

getEmojis :: (MonadError SetupException m, MonadIO m) => m [Emoji]
getEmojis = do
  exists <- liftIO . doesFileExist $ txtPath
  if exists
    then do
      ees <- liftIO (traverse readEither . lines <$> readFile txtPath :: IO (Either String [Emoji]))
      case ees of
        Right es -> pure es
        _        -> throwError ParseException
    else do
      b <- getResponseBody <$> resp
      case b of
        Right es -> liftIO $ writeFile txtPath (unlines . map show $ es) >> pure es
        _        -> throwError ParseException
  where
    req = fromString . concat $
      [ apiHost
      , apiPath
      , "?access_key="
      , myToken
      ]
    resp :: (MonadIO m, MonadError SetupException m) => m (Response (Either SetupException [Emoji]))
    resp = fmap (fmap (first (const APIException))) . httpJSONEither $ req

filterEmojis :: Subgroups -> [Emoji] -> [String]
filterEmojis gs es = [ getEmoji e | e <- es, getEmojiSubgroup e `elem` gs ]
