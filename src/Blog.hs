{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Blog where

import           Text.Blaze (toValue, preEscapedString)
import           Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

blogTemplate :: String -> String -> String -> Html
blogTemplate t h o = H.docTypeHtml $ do
    H.head $ do
        H.title $ (toHtml t)
        H.link ! A.rel "alternate" ! A.type_ "application/rss+xml" ! A.title "RSS-Feed" ! A.href "/rss"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/res/blogstyle.css" ! A.media "all"
        H.meta ! A.httpEquiv "content-type" ! A.content "text/html;charset=UTF-8"
{-        H.style ! A.type_ "text/css" ! A.title "iOS iMessage" ! A.media "screen and (max-device-width: 1024px)" $ "#cosx{display:none;} #cios{display:block;}" -}
    H.body $ do
        H.div ! A.class_ "mainshell" $ H.div ! A.class_ "gradBox" $ H.div ! A.class_ "header" $ do
                H.a ! A.href "/" ! A.style "text-decoration:none;color:black;font-size:x-large;font-weight:bold;" $
                        (toHtml t)
                H.br
                H.span ! A.id "cosx" ! A.style "display:block;" $ H.b $ contactInfo "imessage:tazjin@me.com"
                H.span ! A.id "cios" ! A.style "display:none;" $ H.b $ contactInfo "sms:tazjin@me.com"
    where
        contactInfo (imu :: String) = do
            toHtml h
            H.a ! A.href "mailto:hej@tazj.in" $ "Mail"
            ", "
            H.a ! A.href "http://twitter.com/#!/tazjin" ! A.target "_blank" $ "Twitter"
            toHtml o
            H.a ! A.href (toValue imu) ! A.target "_blank" $ "iMessage"
            "."