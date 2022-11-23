{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.AesonBson
import Database.MongoDB hiding (value)
import Lib.DbConfig
import Lib.ServerOpts
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Options.Applicative
import Web.Scotty
import Text.Read (readMaybe)
import Network.HTTP.Types.Status ( notFound404 )

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

  let run = access pipe master db

  scotty (port serverOpts) $ do
    middleware (if (debug serverOpts) then logStdoutDev else logStdout)

    get "/posts/" $ do
      posts <- run getPosts
      json $ map aesonify posts

    get "/posts/:oid" $ do
      oidParam <- param "oid"

      let oid = (readMaybe oidParam :: Maybe ObjectId)

      maybePost <- run $ findOne (select ["_id" =: oid] "posts")

      case maybePost of Just p -> json $ aesonify p
                        Nothing -> status notFound404 >> text "Not Found"
