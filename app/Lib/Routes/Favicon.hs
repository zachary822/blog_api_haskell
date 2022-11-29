{-# LANGUAGE OverloadedStrings #-}

module Lib.Routes.Favicon where

import Web.Scotty

favicon :: FilePath -> ScottyM ()
favicon icnPath = get "/favicon.ico" $ do
  setHeader "Content-Type" "image/x-icon"
  file icnPath
