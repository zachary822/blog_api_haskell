{-# LANGUAGE OverloadedStrings #-}

module Lib.DbConfig where

import Data.Text qualified as T
import Network.URI

data DbConfig = DbConfig
  { dbhost :: String,
    dbname :: T.Text,
    dbuser :: T.Text,
    dbpasswd :: T.Text
  }
  deriving (Show)

getDbConfig :: String -> Maybe DbConfig
getDbConfig dburi = do
  uri <- parseURI dburi
  ua <- uriAuthority uri
  let user : pass : _ = T.splitOn ":" $ T.dropWhileEnd (== '@') (T.pack $ uriUserInfo ua)

  return
    DbConfig
      { dbhost = uriRegName ua,
        dbname = T.dropWhile (== '/') (T.pack $ uriPath uri),
        dbuser = user,
        dbpasswd = pass
      }
