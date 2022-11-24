{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.AesonBson
import Data.Maybe (fromJust)
import Database.MongoDB hiding (value)
import Lib.DbConfig
import Lib.Post
import Lib.ServerOpts
import Network.HTTP.Types.Status (badRequest400, notFound404)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Options.Applicative (execParser)
import System.Environment (getEnv)
import Text.Read (readMaybe)
import Web.Scotty

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
      limit <- param "limit" `rescue` (\_ -> return 10)
      offset <- param "offset" `rescue` (\_ -> return 0)

      posts <- run $ getPosts limit offset
      json $ map aesonify posts

    get "/posts/:oid" $ do
      oidParam <- param "oid"

      let oid = (readMaybe oidParam :: Maybe ObjectId)

      maybePost <- case oid of
        Just o -> run $ getPost o
        Nothing -> raiseStatus badRequest400 "Bad ObjectId"

      case maybePost of
        Just p -> json $ aesonify p
        Nothing -> raiseStatus notFound404 "Not Found"
