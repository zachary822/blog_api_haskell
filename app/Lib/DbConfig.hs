{-# LANGUAGE OverloadedStrings #-}

module Lib.DbConfig where

import Data.Maybe (fromJust)
import Data.Text qualified as T
import Network.URI
import System.Environment

data DbConfig = DbConfig
  { dbhost :: String,
    dbname :: T.Text,
    dbuser :: T.Text,
    dbpasswd :: T.Text
  }
  deriving (Show)

getDbConfig = do
  dburi <- getEnv "MONGODB_URI"
  let uri = fromJust $ parseURI dburi
  let ua = fromJust $ uriAuthority uri
  let user : pass : _ = T.splitOn ":" $ T.dropWhileEnd (== '@') (T.pack $ uriUserInfo ua)

  return
    DbConfig
      { dbhost = uriRegName ua,
        dbname = T.dropWhile (== '/') (T.pack $ uriPath uri),
        dbuser = user,
        dbpasswd = pass
      }
