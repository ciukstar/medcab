{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Common
  ( getFaviconR, getRobotsR, getSitemapR, getWebAppManifestR
  ) where

import Control.Monad ( Monad(return) )
import Data.Aeson (object, (.=), Value (String))
import Data.Conduit (yield)
import Data.FileEmbed (embedFile)
import Data.Function (($))
import Data.Maybe (Maybe (Nothing, Just))
import Prelude ((*))
import Foundation
    ( Handler
    , Route (HomeR, StaticR)
    , AppMessage (MsgAppName, MsgAppDescription)
    )
import Settings.StaticFiles (img_medical_services_FILL0_wght400_GRAD0_opsz512_png)
import Yesod.Core.Content
    ( TypedContent (TypedContent), typePlain, ToContent (toContent) )
import Yesod.Core.Handler
    ( cacheSeconds, selectRep, getUrlRender, getMessageRender )
import Yesod.Core.Json (array, provideJson)
import Yesod.Sitemap
    (sitemap, SitemapUrl (SitemapUrl), SitemapChangeFreq (Monthly))


getWebAppManifestR :: Handler TypedContent
getWebAppManifestR = do
    urlRender <- getUrlRender
    msgRender <- getMessageRender
    selectRep $ provideJson $ object
        [ "name" .= msgRender MsgAppName
        , "short_name" .= msgRender MsgAppName
        , "description" .= msgRender MsgAppDescription
        , "categories" .= array [String "medical"]
        , "start_url" .= urlRender HomeR
        , "theme_color" .= String "#FFFFFF"
        , "background_color" .= String "#FFFFFF"
        , "display" .= String "standalone"
        , "icons" .= array [ object [ "src" .= urlRender (StaticR img_medical_services_FILL0_wght400_GRAD0_opsz512_png)
                                    , "type" .= String "image/png"
                                    , "sizes" .= String "512x512"
                                    ]
                           , object [ "src" .= urlRender (StaticR img_medical_services_FILL0_wght400_GRAD0_opsz512_png)
                                    , "type" .= String "image/png"
                                    , "sizes" .= String "512x512"
                                    , "purpose" .= String "maskable"
                                    ]
                           ]
        ]


getSitemapR :: Handler TypedContent
getSitemapR = sitemap $ do
    yield $ SitemapUrl HomeR Nothing (Just Monthly) (Just 1.0)


getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
    return $ TypedContent "image/x-icon"
           $ toContent $(embedFile "config/favicon.ico")
