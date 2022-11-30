{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.AesonBson
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Pool
import Data.Text.Lazy qualified as L
import Data.Word (Word32)
import Database.MongoDB
import Lib.DbConfig
import Lib.Middleware
import Lib.Post
import Lib.ServerOpts
import Network.HTTP.Types.Status (notFound404)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Options.Applicative (execParser)
import System.Environment (getEnv)
import Text.Read (readMaybe)
import Web.Scotty
import Web.Scotty.Trans (ActionT)

getOidParam :: L.Text -> MaybeT ActionM ObjectId
getOidParam o = MaybeT $ fmap readMaybe $ param o

getLimitOffset :: ActionT L.Text IO (Limit, Word32)
getLimitOffset = do
  limit <- param "limit" `rescue` (\_ -> return 10)
  offset <- param "offset" `rescue` (\_ -> return 0)
  return (limit, offset)

getMongoPipe :: DbConfig -> IO Pipe
getMongoPipe dbConfig = do
  replica <- openReplicaSetSRV' (dbhost dbConfig)
  pipe <- primary replica
  authSuccess <- access pipe master "admin" (auth (dbuser dbConfig) (dbpasswd dbConfig))
  guard authSuccess
  return pipe

getPipe :: Username -> Password -> ReplicaSet -> IO Pipe
getPipe username passwd replica = do
  pipe <- primary replica
  authSuccess <- access pipe master "admin" (auth username passwd)
  guard authSuccess
  return pipe

getPipePool :: Username -> Password -> ReplicaSet -> IO (Pool Pipe)
getPipePool username passwd replica = newPool $ PoolConfig (getPipe username passwd replica) close 30 5

main :: IO ()
main = do
  serverOpts <- execParser opts
  dburi <- getEnv "MONGODB_URI"
  let dbConfig = fromJust $ getDbConfig dburi
  let db = dbname dbConfig

  replica <- openReplicaSetSRV' (dbhost dbConfig)
  pipePool <- getPipePool (dbuser dbConfig) (dbpasswd dbConfig) replica

  let run q =
        liftIO $
          withResource pipePool $
            ( \pipe -> do
                closed <- isClosed pipe
                guard $ not closed
                access pipe master db q
            )
      runGetPost = MaybeT . run . getPost

  scotty (port serverOpts) $ do
    setMaxRequestBodySize 1024

    middleware removeServer
    middleware $ setCSP (Map.fromList [("img-src", "self")])
    middleware (if (debug serverOpts) then logStdoutDev else logStdout)

    get "/posts" $ do
      (limit, offset) <- getLimitOffset
      posts <- run $ getPosts limit offset
      json $ map aesonify posts

    get (regex "^/posts/([A-Fa-f0-9]{24})$") $ do
      maybePost <- runMaybeT $ do
        o <- getOidParam "1"
        p <- runGetPost o
        return $ aesonify p

      case maybePost of
        Just p -> json p
        Nothing -> raiseStatus notFound404 "Not Found"
