{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.AesonBson
import Data.Maybe
import Database.MongoDB hiding (value)
import Lib.DbConfig
import Lib.ServerOpts
import Network.HTTP.Types.Status (badRequest400, notFound404)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Options.Applicative
import System.Environment
import Text.Read (readMaybe)
import Web.Scotty

getPosts = do
  cur <- find (select [] "posts") {sort = ["updated" =: -1]}
  rest cur

main :: IO ()
main = do
  serverOpts <- execParser opts
  dburi <- getEnv "MONGODB_URI"
  let dbConfig = fromJust $ getDbConfig dburi
  let db = dbname dbConfig

  replica <- openReplicaSetSRV' (dbhost dbConfig)
  pipe <- primary replica
  _ <- access pipe master "admin" (auth (dbuser dbConfig) (dbpasswd dbConfig))

  let run = access pipe master db

  scotty (port serverOpts) $ do
    middleware (if (debug serverOpts) then logStdoutDev else logStdout)

    get "/posts/" $ do
      posts <- run getPosts
      json $ map aesonify posts

    get "/posts/:oid" $ do
      oidParam <- param "oid"

      let oid = (readMaybe oidParam :: Maybe ObjectId)

      maybePost <- case oid of
        Just o -> run $ findOne (select ["_id" =: o] "posts")
        Nothing -> return Nothing

      case maybePost of
        Just p -> json $ aesonify p
        Nothing -> status notFound404 >> text "Not Found"
