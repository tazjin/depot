{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

module Blog where

import           Data.Data (Data, Typeable)
import           Data.List (intersperse)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           System.Locale (defaultTimeLocale)
import           Text.Blaze (toValue, preEscapedText)
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

blogText :: (a -> String) -> a -> Text
blogText f = T.pack . f

intersperse' :: a -> [a] -> [a]
intersperse' sep l = sep : intersperse sep l

blogTemplate :: BlogLang -> Text -> Html -> Html
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
            showFooter lang $ T.pack version
        H.div ! A.class_ "centerbox" $
            H.img ! A.src "http://getpunchd.com/img/june/idiots.png" ! A.alt ""
    where
        contactInfo (imu :: Text) = do
            toHtml $ contactText lang
            H.a ! A.href (toValue mailTo) $ "Mail"
            ", "
            H.a ! A.href (toValue twitter) ! A.target "_blank" $ "Twitter"
            toHtml $ orText lang
            H.a ! A.href (toValue imu) ! A.target "_blank" $ "iMessage"
            "."

renderEntries :: Bool -> [Entry] -> Text -> Maybe Html -> Html
renderEntries showAll entries topText footerLinks = 
    H.div ! A.class_ "innerBox" $ do
        H.div ! A.class_ "innerBoxTop" $ toHtml topText
        H.div ! A.class_ "innerBoxMiddle" $ do
            H.ul $ if' showAll
                (sequence_ $ map showEntry entries)
                (sequence_ . take 6 $ map showEntry entries)
            getFooterLinks footerLinks
    where
        showEntry :: Entry -> Html
        showEntry e = H.li $ do 
            entryLink e
            preEscapedText $ T.concat [" ", blogText text e, "<br>&nbsp;</br>"]
        entryLink e = H.a ! A.href (toValue $ concat $ intersperse' "/" $ linkElems e) $
                        toHtml ("[" ++ show(length $ comments e) ++ "]")
        linkElems e = [show(lang e), show(year e), show(month e), show(day e), _id e]
        getFooterLinks (Just h) = h
        getFooterLinks Nothing = mempty

renderEntry :: Entry -> Html
renderEntry entry = H.div ! A.class_ "innerBox" $ do
    H.div ! A.class_ "innerBoxTop" $ toHtml $ title entry
    H.div ! A.class_ "innerBoxMiddle" $ do
        H.article $ H.ul $ H.li $ do
            preEscapedText $ blogText text entry
            preEscapedText $ blogText mtext entry
        H.div ! A.class_ "innerBoxComments" $ do
            H.div ! A.name "cHead" ! A.style "font-size:large;font-weight:bold;" $ toHtml $ cHead (lang entry)
            H.ul $ renderComments (comments entry) (lang entry)
            renderCommentBox $ lang entry

renderCommentBox :: BlogLang -> Html
renderCommentBox lang = do
    H.div ! A.name "cHead" $ toHtml $ cwHead lang
    H.form $ do
        H.p $ H.label $ do
            toHtml ("Name:" :: Text)
            H.input 
{-
<form>
 <p><label>Customer name: <input></label></p>
</form>
-}

renderComments :: [Comment] -> BlogLang -> Html
renderComments [] lang = H.li $ toHtml $ noComments lang
renderComments comments lang = sequence_ $ map showComment comments
    where
        showComment :: Comment -> Html
        showComment c = H.li $ do
            H.a ! A.name (toValue $ cdate c) ! A.href (toValue $ "#" ++ (show $ cdate c)) ! A.class_ "cl" $
               H.i $ toHtml $ (cauthor c ++ ": ")
            preEscapedText $ blogText ctext c
            H.p ! A.class_ "tt" $ toHtml (timeString $ cdate c)
        getTime :: Integer -> Maybe UTCTime
        getTime t = parseTime defaultTimeLocale "%s" (show t)
        showTime lang (Just t) = formatTime defaultTimeLocale (cTimeFormat lang) t
        showTime _ Nothing = "[???]" -- this can not happen??
        timeString = (showTime lang) . getTime

showLinks :: Maybe Int -> BlogLang -> Html
showLinks (Just i) lang = H.div ! A.class_ "centerbox" $ do
    H.a ! A.href (toValue $ "/?page=" ++ show (i+1)) $ toHtml $ backText lang
    toHtml (" -- " :: Text)
    H.a ! A.href (toValue $ "/?page=" ++ show (i-1)) $ toHtml $ nextText lang
showLinks Nothing lang = H.div ! A.class_ "centerbox" $
    H.a ! A.href "/?page=2" $ toHtml $  backText lang

showFooter :: BlogLang -> Text -> Html
showFooter l v = H.div ! A.class_ "rightbox" ! A.style "text-align:right;" $ do
    toHtml ("Proudly made with " :: Text)
    H.a ! A.href "http://haskell.org" $ "Haskell"
    toHtml (", " :: Text)
    H.a ! A.href "http://couchdb.apache.org/" $ "CouchDB"
    toHtml (" and without PHP, Java, Perl, MySQL and Python." :: Text)
    H.br
    H.a ! A.href (toValue repoURL) $ toHtml $ T.concat ["Version ", v]
    preEscapedText "&nbsp;"
    H.a ! A.href "/notice" $ toHtml $ noticeText l

-- Error pages
showError :: BlogError -> Html
showError _ = undefined
