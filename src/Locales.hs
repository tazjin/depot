module Locales where

import           BlogDB      (BlogLang (..))
import           Data.Maybe  (fromMaybe)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Network.URI

{- to add a language simply define its abbreviation and Show instance then
 - translate the appropriate strings and add CouchDB views in Server.hs -}

data BlogError = NotFound | UnknownError

version = "5.1-beta"

allLang = [EN, DE]

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

blogTitle :: BlogLang -> Text -> Text
blogTitle DE s = T.concat ["Tazjins blog", s]
blogTitle EN s = T.concat ["Tazjin's blog", s]

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
                    1  -> "Januar "
                    2  -> "Februar "
                    3  -> "März "
                    4  -> "April "
                    5  -> "Mai "
                    6  -> "Juni "
                    7  -> "Juli "
                    8  -> "August "
                    9  -> "September "
                    10 -> "Oktober "
                    11 -> "November "
                    12 -> "Dezember "
                    _  -> "Unbekannt "
    monthName EN m = case m of
                    1  -> "January "
                    2  -> "February "
                    3  -> "March "
                    4  -> "April "
                    5  -> "May "
                    6  -> "June "
                    7  -> "July "
                    8  -> "August "
                    9  -> "September "
                    10 -> "October "
                    11 -> "November "
                    12 -> "December "
                    _  -> "Unknown "

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
readMore DE = "[Weiterlesen]"
readMore EN = "[Read more]"

eTimeFormat :: BlogLang -> String
eTimeFormat DE = "Geschrieben am %Y-%m-%d von "
eTimeFormat EN = "Written on %Y-%m-%d by "

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
notFoundText EN = "The requested object could not be found."

unknownErrorText :: BlogLang -> Text
unknownErrorText DE = "Ein unbekannter Fehler ist aufgetreten."
unknownErrorText EN = "An unknown error has occured."

-- static information
repoURL   :: Text = "https://bitbucket.org/tazjin/tazblog-haskell"
mailTo    :: Text = "mailto:tazjin+blog@gmail.com"
twitter   :: Text = "http://twitter.com/#!/tazjin"
