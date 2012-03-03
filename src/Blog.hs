{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

module Blog where

import           Data.Data (Data, Typeable)
import           Data.List (intersperse)
import           Data.Monoid (mempty)
import           Data.Time
import           System.Locale (defaultTimeLocale)
import           Text.Blaze (toValue, preEscapedString)
import           Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Locales

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


intersperse' :: a -> [a] -> [a]
intersperse' sep l = sep : intersperse sep l

blogTemplate :: BlogLang -> String -> Html -> Html
blogTemplate lang t_append body = H.docTypeHtml $ do --add body
    H.head $ do
        H.title $ (toHtml $ blogTitle lang t_append)
        H.link ! A.rel "alternate" ! A.type_ "application/rss+xml" ! A.title "RSS-Feed" ! A.href "/rss"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/res/blogstyle.css" ! A.media "all"
        H.meta ! A.httpEquiv "content-type" ! A.content "text/html;charset=UTF-8"
        --H.style ! A.type_ "text/css" ! A.title "iOS iMessage" ! A.media "screen and (max-device-width: 1024px)" $ "#cosx{display:none;}"
    H.body $ do
        H.div ! A.class_ "mainshell" $ H.div ! A.class_ "gradBox" $ do
            H.div ! A.class_ "header" $ do
                H.a ! A.href "/" ! A.style "text-decoration:none;color:black;font-size:x-large;font-weight:bold;" $
                        toHtml $ blogTitle lang ""
                H.br
                H.span ! A.id "cosx" ! A.style "display:block;" $ H.b $ contactInfo iMessage
               -- H.span ! A.id "cios" ! A.style "display:none;" $ H.b $ contactInfo "sms:tazjin@me.com"
            H.div ! A.class_ "myclear" $ mempty
            body
            H.div ! A.class_ "myclear" $ mempty
            showFooter lang version
        H.div ! A.class_ "centerbox" $
            H.img ! A.src "http://getpunchd.com/img/june/idiots.png" ! A.alt ""
    where
        contactInfo (imu :: String) = do
            toHtml $ contactText lang
            H.a ! A.href (toValue mailTo) $ "Mail"
            ", "
            H.a ! A.href (toValue twitter) ! A.target "_blank" $ "Twitter"
            toHtml $ orString lang
            H.a ! A.href (toValue imu) ! A.target "_blank" $ "iMessage"
            "."

renderEntries :: [Entry] -> String-> Html
renderEntries entries topText = H.div ! A.class_ "innerBox" $ do
    H.div ! A.class_ "innerBoxTop" $ toHtml topText
    H.div ! A.class_ "innerBoxMiddle" $ do
        H.ul $ 
            sequence_ . reverse $ map showEntry entries
    where
        showEntry :: Entry -> Html
        showEntry e = H.li $ do 
            entryLink e
            preEscapedString $ " " ++ (text e) ++ "<br>&nbsp;</br>"
        entryLink e = H.a ! A.href (toValue $ concat $ intersperse' "/" $ linkElems e) $
                        toHtml ("[" ++ show(length $ comments e) ++ "]")
        linkElems e = [show(lang e), show(year e), show(month e), show(day e), _id e]

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

renderComments :: [Comment] -> BlogLang -> Html
renderComments [] lang = H.li $ toHtml $ noComments lang
renderComments comments lang = sequence_ $ map showComment comments
    where
        showComment :: Comment -> Html
        showComment c = H.li $ do
            H.a ! A.name (toValue $ cdate c) ! A.href (toValue $ "#" ++ (show $ cdate c)) ! A.class_ "cl" $
               H.i $ toHtml $ (cauthor c ++ ": ")
            preEscapedString $ ctext c
            H.p ! A.class_ "tt" $ toHtml (timeString $ cdate c)
        getTime :: Integer -> Maybe UTCTime
        getTime t = parseTime defaultTimeLocale "%s" (show t)
        showTime lang (Just t) = formatTime defaultTimeLocale (cTimeFormat lang) t
        showTime _ Nothing = "[???]" -- this can not happen??
        timeString = (showTime lang) . getTime

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

-- Error pages
showError :: BlogError -> Html
showError _ = undefined
