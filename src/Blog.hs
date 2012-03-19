{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, RecordWildCards #-}

module Blog where

import           Control.Monad (when, unless)
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
import           BlogDB

-- custom list functions
intersperse' :: a -> [a] -> [a]
intersperse' sep l = sep : intersperse sep l

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

analytics :: Text
analytics = T.pack $ unlines ["<script type=\"text/javascript\">"
                             ,"  var _gaq = _gaq || [];"
                             ,"  _gaq.push(['_setAccount', 'UA-26042394-1']);"
                             ,"  _gaq.push(['_trackPageview']);"
                             ,"  (function() {"
                             ,"    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;"
                             ,"    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';"
                             ,"    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);"
                             ,"  })();"
                             ,"</script>"]

blogTemplate :: BlogLang -> Text -> Html -> Html
blogTemplate lang t_append body = H.docTypeHtml $ do --add body
    H.head $ do
        H.title $ (toHtml $ blogTitle lang t_append)
        H.link ! A.rel "alternate" ! A.type_ "application/rss+xml" ! A.title "RSS-Feed" ! A.href "/rss"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/res/blogstyle.css" ! A.media "all"
        H.meta ! A.httpEquiv "content-type" ! A.content "text/html;charset=UTF-8"
        --H.style ! A.type_ "text/css" ! A.title "iOS iMessage" ! A.media "screen and (max-device-width: 1024px)" $ "#cosx{display:none;}"
        preEscapedText analytics
    H.body $ do
        H.div ! A.class_ "mainshell" $ H.div ! A.class_ "gradBox" $ do
            H.div ! A.class_ "header" $ do
                H.a ! A.href (toValue $ "/" ++ show lang) ! 
                    A.style "text-decoration:none;color:black;font-size:x-large;font-weight:bold;" $
                        toHtml $ blogTitle lang ""
                H.p ! A.style "clear: both;" $ do
                    H.span ! A.style "float: left;" ! A.id "cosx" $ H.b $ contactInfo iMessage
                   -- H.span ! A.id "cios" ! A.style "display:none;" $ H.b $ contactInfo "sms:tazjin@me.com"
                    H.span ! A.style "float:right;" $ preEscapedText $ rightText lang
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
            entryLink e $ T.pack $ show(length $ comments e)
            preEscapedText $ T.append " " $ btext e
            when ( mtext e /= T.empty ) $
                H.p $ entryLink e $ readMore $ lang e
            unless ( mtext e /= T.empty ) $
                preEscapedText "<br>&nbsp;</br>"
        entryLink :: Entry -> Text -> Html
        entryLink e s = H.a ! A.href (toValue $ concat $ intersperse' "/" $ linkElems e) $
                        toHtml (T.concat ["[", s, "]"])
        linkElems e = [show(lang e), show $ entryId e]
        getFooterLinks (Just h) = h
        getFooterLinks Nothing = mempty

renderEntry :: Entry -> Html
renderEntry (Entry{..}) = H.div ! A.class_ "innerBox" $ do
    H.div ! A.class_ "innerBoxTop" $ toHtml $ title
    H.div ! A.class_ "innerBoxMiddle" $ do
        H.article $ H.ul $ H.li $ do
            preEscapedText $ btext
            H.p $ preEscapedText $ mtext
        H.div ! A.class_ "innerBoxComments" $ do
            H.div ! A.class_ "cHead" $ toHtml $ cHead lang -- ! A.style "font-size:large;font-weight:bold;"
            H.ul $ renderComments comments lang
            renderCommentBox lang entryId

renderCommentBox :: BlogLang -> EntryId -> Html
renderCommentBox cLang cId = do
    H.div ! A.class_ "cHead" $ toHtml $ cwHead cLang
    H.form ! A.method "POST" ! A.action (toValue $ "/" ++ (show cLang) ++  "/postcomment/" ++ show cId) $ do
        H.p $ H.label $ do
            H.span $ "Name:" --toHtml ("Name:" :: String)
            H.input ! A.name "cname"
        H.p $ H.label $ do
            H.span $ toHtml $ cSingle cLang -- toHtml (cSingle lang)
            H.textarea ! A.name "ctext" ! A.cols "50" ! A.rows "13" $ mempty
        H.p $ H.input ! A.type_ "submit" ! A.value (toValue $ cSend cLang)

renderComments :: [Comment] -> BlogLang -> Html
renderComments [] lang = H.li $ toHtml $ noComments lang
renderComments comments lang = sequence_ $ map showComment comments
    where
        showComment :: Comment -> Html
        showComment (Comment{..}) = H.li $ do
            H.i $ toHtml $ T.append cauthor ": "
            preEscapedText ctext
            H.p ! A.class_ "tt" $ toHtml $ timeString cdate
        timeString t = formatTime defaultTimeLocale (cTimeFormat lang) t

showLinks :: Maybe Int -> BlogLang -> Html
showLinks (Just i) lang
    | ( i > 1) = H.div ! A.class_ "centerbox" $ do
        H.a ! A.href (toValue $ "/" ++ show lang ++ "/?page=" ++ show (i+1)) $ 
                                toHtml $ backText lang
        toHtml (" -- " :: Text)
        H.a ! A.href (toValue $ "/" ++ show lang ++ "/?page=" ++ show (i-1)) $
                                toHtml $ nextText lang
    | ( i <= 1 ) = showLinks Nothing lang 
showLinks Nothing lang = H.div ! A.class_ "centerbox" $
    H.a ! A.href (toValue $ "/" ++ show lang ++ "/?page=2") $ 
                                toHtml $  backText lang

showFooter :: BlogLang -> Text -> Html
showFooter l v = H.div ! A.class_ "rightbox" ! A.style "text-align:right;" $ do
    toHtml ("Proudly made with " :: Text)
    H.a ! A.href "http://haskell.org" $ "Haskell"
    toHtml (", " :: Text)
    H.a ! A.href "http://hackage.haskell.org/package/acid-state-0.6.3" $ "Acid-State"
    toHtml (" and without PHP, Java, Perl, MySQL and Python." :: Text)
    H.br
    H.a ! A.href (toValue repoURL) $ toHtml $ T.append "Version " v
    preEscapedText "&nbsp;"
    H.a ! A.href "/notice" $ toHtml $ noticeText l

showSiteNotice :: Html
showSiteNotice = H.docTypeHtml $ do
    H.title $ "Impressum"
    H.h2 $ preEscapedText "Impressum und <a alt=\"Verantwortlich im Sinne des Presserechtes\">ViSdP</a>"
    H.i $ "[German law demands this]"
    H.br
    H.p $ do
        toHtml ("Vincent Ambo" :: Text)
        H.br
        toHtml ("Benfleetstr. 8" :: Text)
        H.br 
        toHtml ("50858 Köln" :: Text)
        H.p $ H.a ! A.href "/" ! A.style "color:black" $ "Back"

{- Administration pages -}

adminTemplate :: Text -> Html -> Html
adminTemplate title body = H.docTypeHtml $ do
    H.head $ do
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/res/admin.css" ! A.media "all"
        H.meta ! A.httpEquiv "content-type" ! A.content "text/html;charset=UTF-8"
        H.title $ toHtml $ T.append "TazBlog Admin: " title
    H.body
        body

adminLogin :: Html
adminLogin = adminTemplate "Login" $
  H.div ! A.class_ "loginBox" $ do
    H.div ! A.class_ "loginBoxTop" $ "TazBlog Admin: Login"
    H.div ! A.class_ "loginBoxMiddle" $ H.form ! A.action "/dologin" ! A.method "post" $ do
        H.p $ "Account ID"
        H.p $ H.input ! A.type_ "text" ! A.style "font-size: 2;" 
            ! A.name "account" ! A.value "tazjin" ! A.readonly "1"
        H.p $ "Passwort"
        H.p $ H.input ! A.type_ "password" ! A.style "font-size: 2;" ! A.name "password"
        H.p $ H.input ! A.alt "Anmelden" ! A.type_ "image" ! A.src "/res/signin.gif"

adminIndex :: Text -> Html
adminIndex sUser = adminTemplate "Index" $
  H.div ! A.style "float: center;" $
    H.form ! A.action "/admin/postentry" ! A.method "POST" $ do
      H.table $ do
        H.tr $ do H.td $ "Titel:"
                  H.td $ H.input ! A.type_ "text" ! A.name "title"
        H.tr $ do H.td $ "Sprache:"
                  H.td $ H.select ! A.name "lang" $ do
                    H.option ! A.value "de" $ "Deutsch"
                    H.option ! A.value "en" $ "Englisch"
        H.tr $ do H.td ! A.style "vertical-align: top;" $ "Text:"
                  H.td $ H.textarea ! A.name "btext" ! A.cols "100" ! A.rows "15" $ mempty
        H.tr $ do H.td ! A.style "vertical-align: top;" $ "Mehr Text:"
                  H.td $ H.textarea ! A.name "mtext" ! A.cols "100" ! A.rows "15" $ mempty
      H.input ! A.type_ "hidden" ! A.name "author" ! A.value (toValue sUser)
      H.input ! A.style "margin-left: 20px" ! A.type_ "submit" ! A.value "Absenden"
      adminFooter

adminFooter :: Html
adminFooter =  H.p $ do 
    preEscapedText "<a href=/>Startseite</a> -- Entrylist: <a href=/admin/entrylist/de>DE</a>"
    preEscapedText " & <a href=/admin/entrylist/en>EN</a> -- <a href=#>Backup</a> (NYI)"

adminEntryList :: [Entry] -> Html
adminEntryList entries = adminTemplate "Entrylist" $
  H.div ! A.style "float: center;" $ do
    H.table $ do
        sequence_ $ map showEntryItem entries
    adminFooter
  where
    showEntryItem :: Entry -> Html
    showEntryItem (Entry{..}) = H.tr $ do
        H.td $ H.a ! A.href (toValue $ "/admin/edit/" ++ show entryId) $ toHtml title
        H.td $ toHtml $ formatTime defaultTimeLocale "[On %D at %H:%M]" edate


editPage :: Entry -> Html
editPage (Entry{..}) = adminTemplate "Index" $
  H.div ! A.style "float: center;" $
    H.form ! A.action "/admin/updateentry" ! A.method "POST" $ do
      H.table $ do
        H.tr $ do H.td $ "Titel:"
                  H.td $ H.input ! A.type_ "text" ! A.name "title" ! A.value (toValue title)
        H.tr $ do H.td ! A.style "vertical-align: top;" $ "Text:"
                  H.td $ H.textarea ! A.name "btext" ! A.cols "100" ! A.rows "15" $ toHtml btext
        H.tr $ do H.td ! A.style "vertical-align: top;" $ "Mehr Text:"
                  H.td $ H.textarea ! A.name "mtext" ! A.cols "100" ! A.rows "15" $ toHtml mtext
      H.input ! A.type_ "hidden" ! A.name "eid" ! A.value (toValue $ unEntryId entryId)
      H.input ! A.style "margin-left: 20px" ! A.type_ "submit" ! A.value "Absenden"
      H.p $ do preEscapedText "<a href=/>Startseite</a> -- Entrylist: <a href=/admin/entrylist/de>DE</a>"
               preEscapedText " & <a href=/admin/entrylist/en>EN</a> -- <a href=#>Backup</a> (NYI)"

-- Error pages
showError :: BlogError -> BlogLang -> Html
showError NotFound l = blogTemplate l (T.append ": " $ notFoundTitle l) $ 
  H.div ! A.class_ "innerBox" $ do
    H.div ! A.class_ "innerBoxTop" $ toHtml $ notFoundTitle l
    H.div ! A.class_ "innerBoxMiddle" $ do
        H.p ! A.class_ "notFoundFace" $ toHtml (":'(" :: Text)
        H.p ! A.class_ "notFoundText" $ toHtml $ notFoundText l
