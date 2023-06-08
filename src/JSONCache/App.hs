{-# LANGUAGE OverloadedStrings #-}
module JSONCache.App
  ( app ) where


import Control.Concurrent.STM (atomically)
import Control.Exception (catch)
import Data.Aeson (encode, decode)
import Data.String (fromString)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai

import JSONCache.HTTP
import JSONCache.State


type Respond = Response -> IO ResponseReceived


app :: State -> Application
app state request respond =
  let handler = case requestMethod request of
        "GET" -> handleGet
        "PUT" -> handlePut
        "PATCH" -> handlePatch
        "DELETE" -> handleDelete
        _ -> handleError methodNotAllowed
   in catchError respond $ handler request state (pathInfo request) respond


catchError :: Respond -> IO ResponseReceived -> IO ResponseReceived
catchError respond action =
  action `catch` handler
  where
    handler :: TypeError -> IO ResponseReceived
    handler (TypeError e) =
      respond $ responseLBS status500 [contentTypeText] $ "TypeError: " <> fromString e


handleGet :: Request -> State -> [T.Text] -> Respond -> IO ResponseReceived
handleGet _ state path respond = do
  v <- atomically $ do
    s <- getChild state path
    getValue s
  respond $ responseLBS status200 [contentTypeJSON] $ encode v


handlePut :: Request -> State -> [T.Text] -> Respond -> IO ResponseReceived
handlePut request state path respond = do
  lbs <- lazyRequestBody request
  case decode lbs of
    Nothing ->
      respond $ responseLBS status400 [contentTypeText] "Invalid JSON"
    Just v -> do
      atomically $ do
        (s, sPath) <- getAncestor state path
        putDescendant s sPath v
      respond $ responseLBS status200 [contentTypeJSON] $ encode v


handlePatch :: Request -> State -> [T.Text] -> Respond -> IO ResponseReceived
handlePatch request state path respond = do
  lbs <- lazyRequestBody request
  case decode lbs of
    Nothing ->
      respond $ responseLBS status400 [contentTypeText] "Invalid JSON"
    Just v -> do
      v' <- atomically $ do
        (s, sPath) <- getAncestor state path
        case sPath of
          [] ->
            patchValue s v
          _:_ ->
            putDescendant s sPath v
        getChild s sPath >>= getValue
      respond $ responseLBS status200 [contentTypeJSON] $ encode v'


handleDelete :: Request -> State -> [T.Text] -> Respond -> IO ResponseReceived
handleDelete _ state path respond = do
  atomically $ deleteValue state path
  respond $ responseLBS status200 [contentTypeJSON] "null"


handleError :: Response -> Request -> State -> [T.Text] -> Respond -> IO ResponseReceived
handleError response _ _ _ respond = respond response
