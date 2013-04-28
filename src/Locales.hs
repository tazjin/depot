{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Locales where

import           Data.Data   (Data, Typeable)
import           Data.Maybe  (fromMaybe)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Network.URI


import           BlogDB      (BlogLang (..))

{- to add a language simply define its abbreviation and Show instance then
 - translate the appropriate strings and add CouchDB views in Server.hs -}

data BlogError = NotFound | DBError

version = "3.5"

allLang = [EN, DE]

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

blogTitle :: BlogLang -> Text -> Text
blogTitle DE s = T.concat ["Tazjins Blog", s]
blogTitle EN s = T.concat ["Tazjin's Blog", s]

showLangText :: BlogLang -> Text
showLangText EN = "en"
showLangText DE = "de"

-- index site headline
topText DE = "Aktuelle Einträge"
topText EN = "Latest entries"

getMonth :: BlogLang -> Int -> Int -> Text
getMonth l y m = T.append (monthName l m) $ T.pack $ show y
  where
    monthName :: BlogLang -> Int -> Text
    monthName DE m = case m of
                    1 -> "Januar "
                    2 -> "Februar "
                    3 -> "März "
                    4 -> "April "
                    5 -> "Mai "
                    6 -> "Juni "
                    7 -> "Juli "
                    8 -> "August "
                    9 -> "September "
                    10 -> "Oktober "
                    11 -> "November "
                    12 -> "Dezember "
    monthName EN m = case m of
                    1 -> "January "
                    2 -> "February "
                    3 -> "March "
                    4 -> "April "
                    5 -> "May "
                    6 -> "June "
                    7 -> "July "
                    8 -> "August "
                    9 -> "September "
                    10 -> "October "
                    11 -> "November "
                    12 -> "December "

entireMonth :: BlogLang -> Text
entireMonth DE = "Ganzer Monat"
entireMonth EN = "Entire month"

backText :: BlogLang -> Text
backText DE = "Früher"
backText EN = "Earlier"

nextText :: BlogLang -> Text
nextText DE = "Später"
nextText EN = "Later"

readMore :: BlogLang -> Text
readMore DE = "Weiterlesen"
readMore EN = "Read more"

eTimeFormat :: BlogLang -> String
eTimeFormat DE = "Geschrieben am %d.%m.%y von "
eTimeFormat EN = "Written on %D by "

-- contact information
contactText :: BlogLang -> Text
contactText DE = "Wer mich kontaktieren will: "
contactText EN = "Get in touch with me: "

orText :: BlogLang -> Text
orText DE = " oder "
orText EN = " or "

-- footer
noticeText :: BlogLang -> Text
noticeText EN = "site notice"
noticeText DE = "Impressum"

-- comments
noComments :: BlogLang -> Text
noComments DE = " Keine Kommentare"
noComments EN = " No comments yet"

cHead :: BlogLang -> Text
cHead DE = "Kommentare"
cHead EN = "Comments"

cwHead :: BlogLang -> Text
cwHead DE = "Kommentieren:"
cwHead EN = "Comment:"

cSingle :: BlogLang -> Text
cSingle DE = "Kommentar:" --input label
cSingle EN = "Comment:"

cTimeFormat :: BlogLang -> String --formatTime expects a String
cTimeFormat DE = "[Am %d.%m.%y um %H:%M Uhr]"
cTimeFormat EN = "[On %D at %H:%M]"

cSend :: BlogLang -> Text
cSend DE = "Absenden"
cSend EN = "Submit"

cTextPlaceholder :: BlogLang -> Text
cTextPlaceholder DE = "Kommentartext hier eingeben :]"
cTextPlaceholder EN = "Enter your comment here :]"

-- RSS Strings
rssTitle :: BlogLang -> String
rssTitle DE = "Tazjins Blog"
rssTitle EN = "Tazjin's Blog"

rssDesc :: BlogLang -> String
rssDesc DE = "Feed zu Tazjins Blog"
rssDesc EN = "Feed for Tazjin's Blog"

rssLink :: BlogLang -> URI
rssLink l = fromMaybe nullURI $ parseURI ("http://tazj.in/" ++ show l)

-- errors
notFoundTitle :: BlogLang -> Text
notFoundTitle DE = "Nicht gefunden"
notFoundTitle EN = "Not found"

notFoundText :: BlogLang -> Text
notFoundText DE = "Das gewünschte Objekt wurde leider nicht gefunden."
notFoundText EN = "The requested object could unfortunately not be found."

-- right side text (this is inserted AS IS. Escape HTML!)
rightText :: BlogLang -> Text
rightText DE = "English version <a href=\"/en\" class=\"link\">available here</a>."
rightText EN = "Deutsche Version <a href=\"/de\" class=\"link\">hier verf&uuml;gbar</a>."

-- static information
repoURL   :: Text = "https://bitbucket.org/tazjin/tazblog-haskell"
mailTo    :: Text = "mailto:tazjin@gmail.com"
twitter   :: Text = "http://twitter.com/#!/tazjin"
