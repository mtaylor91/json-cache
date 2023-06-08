{-# LANGUAGE OverloadedStrings #-}
module JSONCache
  ( app
  , server
  ) where

import Control.Concurrent.STM (atomically)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)

import JSONCache.App (app)
import JSONCache.State (State(..), initState)


server :: IO ()
server = atomically initState >>= serverState


serverState :: State -> IO ()
serverState state = do
  envHost <- lookupEnv "HOST"
  envPort <- lookupEnv "PORT"
  let bindHost = host envHost
      bindPort = port envPort
  putStrLn $ "Listening on " ++ bindHost ++ ":" ++ show bindPort
  runSettings (settings (fromString bindHost) bindPort) $ app state
  where
    host = fromMaybe "127.0.0.1"
    port = maybe 8080 read
    settings bindHost bindPort = setHost bindHost $ setPort bindPort defaultSettings
