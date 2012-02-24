{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Locales where

import           Data.Data (Data, Typeable)

{- to add a language simply define it's abbreviation and show instance then
 - translate the appropriate strings and add CouchDB views in Server.hs -}

data BlogLang = EN | DE deriving (Data, Typeable)

instance Show BlogLang where
    show EN = "en"
    show DE = "de"

version = ("2.2b" :: String)

allLang = [EN, DE]

blogTitle DE = "Tazjins Blog"
blogTitle EN = "Tazjin's Blog"

-- index site headline
topText DE = "Aktuelle Einträge"
topText EN = "Latest entries"

getMonth :: BlogLang -> Int -> Int -> String
getMonth l y m = monthName l m ++ show y
  where
    monthName :: BlogLang -> Int -> String
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
                    11 -> "November"
                    12 -> "Dezember"
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

entireMonth DE = "Ganzer Monat"
entireMonth EN = "Entire month"

prevMonth DE = "Früher"
prevMonth EN = "Earlier"

nextMonth DE = "Später"
nextMonth EN = "Later"

-- contact information
contactText DE = "Wer mich kontaktieren will: "
contactText EN = "Get in touch with me: "

orString DE = " oder "
orString EN = " or "

-- footer
noticeText EN = "site notice"
noticeText DE = "Impressum"

-- comments
noComments DE = " Keine Kommentare"
noComments EN = " No comments yet"

cHead DE = "Kommentare:"
cHead EN = "Comments:"

cTimeFormat DE = "[Am %d.%m.%y um %H:%M Uhr]"
cTimeFormat EN = "[On %D at %H:%M]"

-- right side text (this is inserted AS IS. Escape HTML!)
rightText DE = "English version <a href=\"en\">available here</a>"
rightText EN = "Deutsche Version <a href=\"de\">hier verf&uuml;gbar</a>"

-- static information
repoURL = "https://bitbucket.org/tazjin/tazblog-haskell"
mailTo  = "mailto:hej@tazj.in"
twitter = "http://twitter.com/#!/tazjin"
iMessage = "imessage:tazjin@me.com"
iMessage' = "sms:tazjin@me.com"