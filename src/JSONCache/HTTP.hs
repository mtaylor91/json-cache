{-# LANGUAGE OverloadedStrings #-}
module JSONCache.HTTP
  ( contentTypeJSON
  , contentTypeText
  , contentTypeYAML
  , methodNotAllowed
  , notFound
  , notImplemented
  ) where

import Network.HTTP.Types
import Network.Wai


contentTypeJSON :: Header
contentTypeJSON = ("Content-Type", "application/json")


contentTypeText :: Header
contentTypeText = ("Content-Type", "text/plain")


contentTypeYAML :: Header
contentTypeYAML = ("Content-Type", "application/x-yaml")


methodNotAllowed :: Response
methodNotAllowed = responseLBS status405 [contentTypeText] "Method Not Allowed"


notFound :: Response
notFound = responseLBS status404 [("Content-Type", "text/plain")] "Not Found"


notImplemented :: Response
notImplemented = responseLBS status501 [contentTypeText] "Not Implemented"
