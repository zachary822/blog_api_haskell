{-# LANGUAGE OverloadedStrings #-}

module Lib.Middleware (setCSP, removeServer) where

import Data.List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (hServer)
import Network.URI
import Network.Wai
import Text.Printf

removeServer :: Middleware
removeServer = modifyResponse $ mapResponseHeaders $ ((hServer, "") :)

unQuoted :: String -> Bool
unQuoted = (||) <$> isURI <*> isInfixOf "*"

setCSP :: Map.Map String String -> Middleware
setCSP policy = modifyResponse $ mapResponseHeaders $ (("Content-Security-Policy", encodeUtf8 $ T.pack value) :)
  where
    value =
      intercalate "; " $
        Map.foldlWithKey
          ( \xs k v ->
              (if unQuoted v then k ++ " " ++ v else printf "%s '%s'" k v :: String) : xs
          )
          []
          policy
