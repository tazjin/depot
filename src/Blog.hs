{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

module Blog where

--import           Control.Monad(when)
import           Data.Data (Data, Typeable)
import           Data.Monoid (mempty)
import           Data.Time
import           System.Locale (defaultTimeLocale)
import           Text.Blaze (toValue, preEscapedString)
import           Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


data Comment = Comment{
    cauthor :: String,
    ctext   :: String,
    cdate   :: Integer
} deriving (Show, Data, Typeable)

data Entry = Entry{
    _id      :: String,
    year     :: Int,
    month    :: Int,
    day      :: Int,
    lang     :: BlogLang,
    title    :: String,
    author   :: String,
    text     :: String,
    mtext    :: String,
    comments :: [Comment]
} deriving (Show, Data, Typeable)

data BlogError = NoEntries | NotFound | DBError

data BlogLang = EN | DE deriving (Data, Typeable)

instance Show BlogLang where
    show EN = "en"
    show DE = "de"

repoURL = ("https://bitbucket.org/tazjin/tazblog-haskell" :: String)

blogTemplate :: String -> String -> String -> String -> BlogLang -> Html -> Html
blogTemplate title ctext1 ortext version lang body = H.docTypeHtml $ do --add body
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
            body
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

renderEntry :: Entry -> Html
renderEntry entry = H.div ! A.class_ "innerBox" $ do
    H.div ! A.class_ "innerBoxTop" $ toHtml $ title entry
    H.div ! A.class_ "innerBoxMiddle" $ do
        H.article $ H.ul $ H.li $ do
            preEscapedString $ text entry
            preEscapedString $ mtext entry
        H.div ! A.class_ "innerBoxComments" $ do
            H.div ! A.name "cHead" ! A.style "font-size:large;font-weight:bold;" $ toHtml $ cHead (lang entry)
            H.ul $ renderComments (comments entry) (lang entry)
    where
        cHead EN = ("Comments:" :: String)
        cHead DE = ("Kommentare:" :: String)

renderComments :: [Comment] -> BlogLang -> Html
renderComments [] DE = H.li $ toHtml (" Keine Kommentare" :: String)
renderComments [] EN = H.li $ toHtml (" No comments yet" :: String)
renderComments comments _ = sequence_ $ map showComment comments
    where
        showComment :: Comment -> Html
        showComment c = H.li $ do
            H.a ! A.name (toValue $ cdate c) ! A.href (toValue $ "#" ++ show c) $
               H.i $ toHtml $ (cauthor c ++ ": ")
            preEscapedString $ ctext c
        getTime :: Integer -> Maybe UTCTime
        getTime = parseTime defaultTimeLocale "%s" (show )
        showTime (Just t) = formatTime defaultTimeLocale "[Am %d.%m.%y um %H:%M Uhr]" t
        showTime Nothing = "???" -- this can not happen??

--[Am %d.%m.%y um %H:%M Uhr]

emptyTest :: BlogLang -> Html
emptyTest lang = H.div ! A.class_ "innerBox" $ do
    H.div ! A.class_ "innerBoxTop" $ "Test"
    H.div ! A.class_ "innerBoxMiddle" $ getTestText lang
    H.div ! A.class_ "myclear" $ mempty
  where
    getTestText DE = toHtml ("Das ist doch schonmal was." :: String)
    getTestText EN = toHtml ("This is starting to look like something." :: String)

showFooter :: BlogLang -> String -> Html
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
    noticeText :: BlogLang -> String
    noticeText EN = "site notice"
    noticeText DE = "Impressum"


-- Error pages
showError :: BlogError -> Html
showError _ = undefined
