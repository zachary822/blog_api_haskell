{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Post where

import Database.MongoDB

getPosts limit offset = do
  cur <- find (select ["published" =: True] "posts") {sort = ["updated" =: -1], limit = limit, skip = offset}
  rest cur

getPost oid = findOne (select ["_id" =: oid, "published" =: True] "posts")
