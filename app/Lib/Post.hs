{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Post where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Word (Word32)
import Database.MongoDB

getPosts :: MonadIO m => Limit -> Word32 -> Control.Monad.Trans.Reader.ReaderT MongoContext m [Document]
getPosts limit offset = do
  cur <- find (select ["published" =: True] "posts") {sort = ["updated" =: -1], limit = limit, skip = offset}
  rest cur

getPost :: (MonadIO m, Val v) => v -> Action m (Maybe Document)
getPost oid = findOne (select ["_id" =: oid, "published" =: True] "posts")
