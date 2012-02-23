{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import           Control.Monad (msum, mzero)
import           Data.Monoid (mempty)
import           Data.ByteString.Char8 (ByteString)
import           Data.Text hiding (map, length, zip, head)
import           Data.Time
import           Database.CouchDB
import           Happstack.Server
import           Network.CGI (liftIO)
import           Text.Blaze (toValue, preEscapedString)
import           Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.JSON.Generic

import           Blog

tmpPolicy :: BodyPolicy
tmpPolicy = (defaultBodyPolicy "./tmp/" 0 1000 1000)


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
                      \(day :: Int) -> path $ \(id_ :: String) -> showEntry year month day id_
         , do nullDir
              ok $ showIndex lang
         ]

showEntry :: Int -> Int -> Int -> String -> ServerPart Response
showEntry y m d i = do
    entryJS <- liftIO $ runCouchDB' $ getDoc (db "tazblog") (doc i)
    let entry = maybeDoc entryJS
    ok $ tryEntry entry

tryEntry :: Maybe Entry -> Response
tryEntry Nothing = toResponse $ showError NotFound
tryEntry (Just entry) = toResponse $ renderBlog eLang $ renderEntry entry
    where
        eLang = lang entry

showIndex :: BlogLang -> Response
showIndex lang = toResponse $ renderBlogHeader lang

renderBlog :: BlogLang -> Html -> Html
renderBlog DE body = blogTemplate "Tazjins Blog" "Wer mich kontaktieren will: " " oder " version DE body
renderBlog EN body = blogTemplate "Tazjin's Blog" "Get in touch with me: " " or " version EN body

renderBlogHeader :: BlogLang -> Html
renderBlogHeader DE = blogTemplate "Tazjins Blog" "Wer mich kontaktieren will: " " oder " version DE (emptyTest DE)
renderBlogHeader EN = blogTemplate "Tazjin's Blog" "Get in touch with me: " " or " version EN (emptyTest EN)

-- http://tazj.in/2012/02/10.155234

-- CouchDB functions
maybeDoc :: Data a => Maybe (Doc, Rev, JSValue) -> Maybe a
maybeDoc (Just(_,_,v)) = Just( stripResult $ fromJSON v)
maybeDoc Nothing = Nothing

stripResult :: Result a -> a
stripResult (Ok z) = z
stripResult (Error s) = error $ "JSON error: " ++ s
-- CouchDB View Setup
latestDEView = "function(doc){ if(doc.lang == \"DE\"){ emit([doc.year, doc.month, doc.day, doc.id_], doc); } }"
latestENView = "function(doc){ if(doc.lang == \"EN\"){ emit([doc.year, doc.month, doc.day, doc.id_]], doc); } }"

latestDE = ViewMap "latestDE" latestDEView
latestEN = ViewMap "latestEN" latestENView

setupBlogViews :: IO ()
setupBlogViews = runCouchDB' $ 
    newView "tazblog" "entries" [latestDE, latestEN]
