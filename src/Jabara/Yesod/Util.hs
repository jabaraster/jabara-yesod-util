{-# LANGUAGE OverloadedStrings #-}
module Jabara.Yesod.Util (
    isAuthenticated
  , getResourcePath
  , getRequestPath

  , tc
  , ttc

  , lookupQueryStringParameterValue
) where

import Data.Aeson (ToJSON, encode)
import Data.List as L (lookup)
import Data.Maybe (maybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Base
import Network.Wai (Request(rawPathInfo))
import Yesod.Auth (YesodAuth, maybeAuthId)
import Yesod.Core (HandlerT, MonadHandler, Route, HandlerSite
                  , YesodRequest(..), getUrlRender, getRequest)
import Yesod.Core (Content)
import Yesod.Core.Content (TypedContent(..), ToContent(..), ToTypedContent(..))

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

tc :: ToJSON a => a -> Content
tc = toContent . encode

ttc :: ToContent a => a -> TypedContent
ttc = TypedContent "application/json" . toContent

lookupQueryStringParameterValue :: MonadHandler m => Text -> m (Maybe Text)
lookupQueryStringParameterValue parameterName = getRequest
    >>= return . L.lookup parameterName . reqGetParams
