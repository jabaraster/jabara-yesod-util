module Jabara.Yesod.Util (
  isAuthenticated
  , getResourcePath
  , getRequestPath
) where

import Data.Maybe (maybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Base
import Network.Wai (Request(rawPathInfo))
import Yesod.Core (HandlerT, MonadHandler, Route, HandlerSite
                  , YesodRequest(..), getUrlRender, getRequest)
import Yesod.Auth (YesodAuth, maybeAuthId)

isAuthenticated :: YesodAuth master => HandlerT master IO Bool
isAuthenticated = maybeAuthId >>= return . maybe False (\_ -> True)

getResourcePath :: MonadHandler m => Route (HandlerSite m) -> m Text
getResourcePath resource = do
    renderFunction <- getUrlRender
    return $ renderFunction resource

getRequestPath :: MonadHandler m => m Text
getRequestPath = getRequest
    >>= return . reqWaiRequest
    >>= return . rawPathInfo
    >>= return . decodeUtf8
