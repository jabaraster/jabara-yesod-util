{-# LANGUAGE OverloadedStrings #-}
module Jabara.Yesod.Middleware (
    csrfMiddleware
) where

import GHC.Base
import Data.Textual.Encoding (decodeUtf8)
import Data.Text (Text, isSuffixOf)
import Network.Wai (Request(rawPathInfo))
import Yesod (Yesod)
import Yesod.Core (HandlerT, Route, getUrlRender, defaultCsrfMiddleware)

import Jabara.Yesod.Util (getRequestPath)

csrfMiddleware :: Yesod site =>
    Route site -> HandlerT site IO res -> HandlerT site IO res
csrfMiddleware logoutRoute  handler = do
    path <- getRequestPath
    renderFunc <- getUrlRender
    logoutPath <- return $ renderFunc logoutRoute
    if isSuffixOf path logoutPath then
        handler
      else
        defaultCsrfMiddleware handler
