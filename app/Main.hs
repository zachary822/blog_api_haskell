{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.AesonBson
import Data.Text qualified as T
import Database.MongoDB hiding (value)
import Lib.DbConfig
import Lib.ServerOpts
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Options.Applicative
import Web.Scotty

getPosts = do
  cur <- find (select [] "posts") {sort = ["updated" =: -1]}
  rest cur

main :: IO ()
main = do
  serverOpts <- execParser opts
  dbConfig <- getDbConfig

  let db = dbname dbConfig

  replica <- openReplicaSetSRV' (dbhost dbConfig)
  pipe <- primary replica
  _ <- access pipe master "admin" (auth (dbuser dbConfig) (dbpasswd dbConfig))

  scotty (port serverOpts) $ do
    middleware (if (debug serverOpts) then logStdoutDev else logStdout)

    get "/" $ do
      posts <- access pipe master db getPosts
      json $ map aesonify posts
