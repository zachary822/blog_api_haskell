{-# LANGUAGE OverloadedStrings #-}

module Lib.Middleware (setCSP, removeServer) where

import Data.ByteString
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (hServer)
import Network.URI
import Network.Wai

removeServer :: Middleware
removeServer = modifyResponse $ mapResponseHeaders $ ((hServer, "") :)

unQuoted :: ByteString -> Bool
unQuoted = (||) <$> (isURI . T.unpack . decodeUtf8) <*> isInfixOf "*"

setCSP :: Map.Map ByteString ByteString -> Middleware
setCSP policy = modifyResponse $ mapResponseHeaders $ (("Content-Security-Policy", value) :)
  where
    value =
      intercalate "; " $
        Map.foldlWithKey
          ( \xs k v ->
              (if unQuoted v then k <> " " <> v else k <> " '" <> v <> "'") : xs
          )
          []
          policy
