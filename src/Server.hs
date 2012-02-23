{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

module Main where

import           Control.Monad (msum, mzero)
import           Data.Data (Data, Typeable)
import           Data.Monoid (mempty)
import           Data.ByteString.Char8 (ByteString)
import           Data.Text hiding (map, length, zip, head)
import           Data.Time
import           Database.CouchDB
import           Happstack.Server
import           Text.Blaze (toValue, preEscapedString)
import           Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.JSON.Generic

import           Blog

tmpPolicy :: BodyPolicy
tmpPolicy = (defaultBodyPolicy "./tmp/" 0 1000 1000)


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

data BlogLang = EN | DE deriving (Data, Typeable)

instance Show BlogLang where
    show EN = "en"
    show DE = "de"

--TazBlog version
version = ("2.2b" :: String)

main :: IO()
main = do
    putStrLn ("TazBlog " ++ version ++ " in Haskell starting")
    simpleHTTP nullConf tazBlog

tazBlog :: ServerPart Response
tazBlog = do
    msum [ dir "en" $ blogHandler EN
         , dir "de" $ blogHandler DE
         , do nullDir
              ok $ showIndex DE
         , do dir " " $ nullDir
              seeOther ("https://plus.google.com/115916629925754851590" :: String) (toResponse ())
         , dir "res" $ serveDirectory DisableBrowsing [] "../res"
         , serveDirectory DisableBrowsing [] "../res"
         ]

blogHandler :: BlogLang -> ServerPart Response
blogHandler lang = 
    msum [ path $ \(year :: Int) -> path $ \(month :: Int) -> path $ --single entry
                      \(day :: Int) -> path $ \(id_ :: String) -> showEntry lang year month day id_
         , do nullDir
              ok $ showIndex lang
         ]

showEntry :: BlogLang -> Int -> Int -> Int -> String -> ServerPart Response
showEntry EN y m d i = undefined
showEntry DE y m d i = undefined

showIndex :: BlogLang -> Response
showIndex lang = toResponse $ renderBlogHeader lang

renderBlogHeader :: BlogLang -> Html
renderBlogHeader DE = blogTemplate "Tazjins Blog" "Wer mich kontaktieren will: " " oder " "de" version
renderBlogHeader EN = blogTemplate "Tazjin's Blog" "Get in touch with me: " " or " "en" version

-- http://tazj.in/2012/02/10.155234

-- CouchDB View Setup
latestDEView = "function(doc){ if(doc.lang == \"de\"){ emit([doc.year, doc.month, doc.day, doc.id_], doc); } }"
latestENView = "function(doc){ if(doc.lang == \"en\"){ emit([doc.year, doc.month, doc.day, doc.id_]], doc); } }"

latestDE = ViewMap "latestDE" latestDEView
latestEN = ViewMap "latestEN" latestENView

setupBlogViews :: IO ()
setupBlogViews = runCouchDB' $ 
    newView "tazblog" "entries" [latestDE, latestEN]
