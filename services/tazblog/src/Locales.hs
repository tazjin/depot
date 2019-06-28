module Locales where

import           BlogDB      (BlogLang (..))
import           Data.Maybe  (fromMaybe)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Network.URI

data BlogError = NotFound | UnknownError

version = "5.1.2"

blogTitle :: BlogLang -> Text -> Text
blogTitle DE s = T.concat ["Tazjins blog", s]
blogTitle EN s = T.concat ["Tazjin's blog", s]

showLangText :: BlogLang -> Text
showLangText EN = "en"
showLangText DE = "de"

backText :: BlogLang -> Text
backText DE = "Früher"
backText EN = "Earlier"

nextText :: BlogLang -> Text
nextText DE = "Später"
nextText EN = "Later"

readMore :: BlogLang -> Text
readMore DE = "[Weiterlesen]"
readMore EN = "[Read more]"

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
twitter   :: Text = "https://twitter.com/tazjin"
