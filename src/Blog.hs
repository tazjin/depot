{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Blog where

import           Data.Monoid (mempty)
import           Text.Blaze (toValue, preEscapedString)
import           Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A




repoURL = ("" :: String)

{-
</div>
<div style=\"text-align:right;\">
Proudly made with 
<a href=\"http://golang.org\">Google Go</a> and without PHP, Java, Perl, MySQL and Python.
<br>Idee zum simplen Blog von 
<a href=\"http://blog.fefe.de\" target=\"_blank\">Fefe</a>
<br>Version 2.1.3&nbsp;
<a href=\"/impressum\">Impressum</a>
</div>
</div>
</div>
<div class=\"centerbox\"><img src=\"http://getpunchd.com/img/june/idiots.png\" alt=\"\"></div>
</body>
</html>"

-}

blogTemplate :: String -> String -> String -> String -> String -> Html -- -> Html
blogTemplate title ctext1 ortext lang version = H.docTypeHtml $ do --add body
    H.head $ do
        H.title $ (toHtml title)
        H.link ! A.rel "alternate" ! A.type_ "application/rss+xml" ! A.title "RSS-Feed" ! A.href "/rss"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/res/blogstyle.css" ! A.media "all"
        H.meta ! A.httpEquiv "content-type" ! A.content "text/html;charset=UTF-8"
        --H.style ! A.type_ "text/css" ! A.title "iOS iMessage" ! A.media "screen and (max-device-width: 1024px)" $ "#cosx{display:none;}"
    H.body $ do
        H.div ! A.class_ "mainshell" $ H.div ! A.class_ "gradBox" $ do
            H.div ! A.class_ "header" $ do
                H.a ! A.href "/" ! A.style "text-decoration:none;color:black;font-size:x-large;font-weight:bold;" $
                        (toHtml title)
                H.br
                H.span ! A.id "cosx" ! A.style "display:block;" $ H.b $ contactInfo "imessage:tazjin@me.com"
               -- H.span ! A.id "cios" ! A.style "display:none;" $ H.b $ contactInfo "sms:tazjin@me.com"
            H.div ! A.class_ "myclear" $ mempty
            emptyTest lang
            showFooter lang version
        H.div ! A.class_ "centerbox" $
            H.img ! A.src "http://getpunchd.com/img/june/idiots.png" ! A.alt ""
    where
        contactInfo (imu :: String) = do
            toHtml ctext1
            H.a ! A.href "mailto:hej@tazj.in" $ "Mail"
            ", "
            H.a ! A.href "http://twitter.com/#!/tazjin" ! A.target "_blank" $ "Twitter"
            toHtml ortext
            H.a ! A.href (toValue imu) ! A.target "_blank" $ "iMessage"
            "."

emptyTest :: String -> Html
emptyTest lang = H.div ! A.class_ "innerBox" $ do
    H.div ! A.class_ "innerBoxTop" $ "Test"
    H.div ! A.class_ "innerBoxMiddle" $ getTestText lang
    H.div ! A.class_ "myclear" $ mempty
  where
    getTestText "de" = toHtml ("Das ist doch schonmal was." :: String)
    getTestText "en" = toHtml ("This is starting to look like something." :: String)

showFooter :: String -> String -> Html
showFooter l v = H.div ! A.class_ "rightbox" ! A.style "text-align:right;" $ do
    toHtml ("Proudly made with " :: String)
    H.a ! A.href "http://haskell.org" $ "Haskell"
    toHtml (", " :: String)
    H.a ! A.href "http://couchdb.apache.org/" $ "CouchDB"
    toHtml (" and without PHP, Java, Perl, MySQL and Python." :: String)
    H.br
    H.a ! A.href (toValue repoURL) $ toHtml $ ("Version " :: String) ++ v
    preEscapedString "&nbsp;"
    H.a ! A.href "/notice" $ toHtml $ noticeText l
  where
    noticeText :: String -> String
    noticeText "en" = "site notice"
    noticeText "de" = "Impressum"
