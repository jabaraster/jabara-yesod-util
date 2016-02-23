{-# LANGUAGE OverloadedStrings #-}
module Jabara.Yesod.Middleware (
    csrfMiddleware
) where

import Prelude hiding (mapM)
import GHC.Base (mapM)
import Data.Text (Text, isSuffixOf)
import Network.Wai (Request(rawPathInfo))
import Yesod (Yesod)
import Yesod.Core (HandlerT, Route, getUrlRender, defaultCsrfMiddleware)

import Jabara.Yesod.Util (getRequestPath)

csrfMiddleware :: Yesod site => [Route site] -> HandlerT site IO res -> HandlerT site IO res
csrfMiddleware uncheckTargetRoutes handler = do
    path <- getRequestPath
    unchecks  <- toText uncheckTargetRoutes
    if any (isSuffixOf path) unchecks then
        handler
      else
        defaultCsrfMiddleware handler
  where
    toText :: Yesod site => [Route site] -> HandlerT site IO [Text]
    toText routes = do
       renderFunc <- getUrlRender
       mapM (return . renderFunc) routes
