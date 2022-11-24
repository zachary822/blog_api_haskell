{-# LANGUAGE OverloadedStrings #-}

module Lib.Middleware where

import Network.HTTP.Types (hServer)
import Network.Wai

removeServer :: Middleware
removeServer = modifyResponse $ mapResponseHeaders $ ((hServer, "") :)
