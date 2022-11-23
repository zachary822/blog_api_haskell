module Lib.ServerOpts where

import Options.Applicative

data ServerOpts = ServerOpts
  { port :: Int,
    debug :: Bool
  }
  deriving (Show)

server :: Parser ServerOpts
server =
  ServerOpts
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> showDefault
          <> value 5000
      )
    <*> switch
      ( long "debug"
      )

opts :: ParserInfo ServerOpts
opts = info (server <**> helper) (fullDesc)
