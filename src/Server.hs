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
import           Locales

tmpPolicy :: BodyPolicy
tmpPolicy = (defaultBodyPolicy "./tmp/" 0 1000 1000)

main :: IO()
main = do
    putStrLn ("TazBlog " ++ version ++ " in Haskell starting")
    simpleHTTP nullConf tazBlog

tazBlog :: ServerPart Response
tazBlog = do
    msum [ dir (show DE) $ blogHandler DE
         , dir (show EN) $ blogHandler EN
         , do nullDir
              showIndex DE
         , do dir " " $ nullDir
              seeOther ("https://plus.google.com/115916629925754851590" :: String) (toResponse ())
         , dir "res" $ serveDirectory DisableBrowsing [] "../res"
         , serveDirectory DisableBrowsing [] "../res"
         ]

blogHandler :: BlogLang -> ServerPart Response
blogHandler lang = 
    msum [ path $ \(year :: Int) -> path $ \(month :: Int) -> path $ --single entry
                      \(day :: Int) -> path $ \(id_ :: String) -> showEntry year month day id_
         , path $ \(year :: Int) -> path $ \(month :: Int) -> path $ 
                      \(day :: Int) -> showDay year month day lang
         , path $ \(year :: Int ) -> path $ \(month :: Int) -> showMonth year month lang
         , path $ \(year :: Int ) -> showYear year lang
         , do nullDir
              showIndex lang
         ]

showEntry :: Int -> Int -> Int -> String -> ServerPart Response
showEntry y m d i = do
    entryJS <- liftIO $ runCouchDB' $ getDoc (db "tazblog") (doc i)
    let entry = maybeDoc entryJS
    ok $ tryEntry entry

tryEntry :: Maybe Entry -> Response
tryEntry Nothing = toResponse $ showError NotFound
tryEntry (Just entry) = toResponse $ blogTemplate eLang $ renderEntry entry
    where
        eLang = lang entry

showIndex :: BlogLang -> ServerPart Response
showIndex lang = do
    entries <- getLatest lang []
    ok $ toResponse $ blogTemplate lang $ renderEntries entries 6 (topText lang)

showDay :: Int -> Int -> Int -> BlogLang -> ServerPart Response
showDay y m d lang = undefined

showMonth :: Int -> Int -> BlogLang -> ServerPart Response
showMonth y m lang = do
    entries <- getLatest lang $ makeQuery startkey endkey
    ok $ toResponse $ blogTemplate lang $ renderEntries entries (length entries) $ getMonth lang y  m
  where
    startkey = JSArray [toJSON y, toJSON m]
    endkey = JSArray [toJSON y, toJSON m, JSObject (toJSObject [] )]

showYear :: Int -> BlogLang -> ServerPart Response
showYear y lang = undefined


-- http://tazj.in/2012/02/10.155234

-- CouchDB functions
getLatest :: BlogLang -> [(String, JSValue)] -> ServerPart [Entry]
getLatest lang arg = do
        queryResult <- queryDB view arg
        let entries = map (stripResult . fromJSON . snd) queryResult
        return entries
    where
        view = case lang of
                EN -> "latestEN"
                DE -> "latestDE"

makeQuery :: JSON a => a -> a -> [(String, JSValue)]
makeQuery qsk qek = [("startkey", (showJSON qsk))
                    ,("endkey", (showJSON qek))]

queryDB :: JSON a => String -> [(String, JSValue)] -> ServerPart [(Doc, a)]
queryDB view arg = liftIO $ runCouchDB' $ queryView (db "tazblog") (doc "entries") (doc view) arg

maybeDoc :: Data a => Maybe (Doc, Rev, JSValue) -> Maybe a
maybeDoc (Just(_,_,v)) = Just( stripResult $ fromJSON v)
maybeDoc Nothing = Nothing

stripResult :: Result a -> a
stripResult (Ok z) = z
stripResult (Error s) = error $ "JSON error: " ++ s

-- CouchDB View Setup
latestDEView = "function(doc){ if(doc.lang == 'DE'){ emit([doc.year, doc.month, doc.day, doc.id_], doc); } }"
latestENView = "function(doc){ if(doc.lang == 'EN'){ emit([doc.year, doc.month, doc.day, doc.id_], doc); } }"

latestDE = ViewMap "latestDE" latestDEView
latestEN = ViewMap "latestEN" latestENView

setupBlogViews :: IO ()
setupBlogViews = runCouchDB' $ 
    newView "tazblog" "entries" [latestDE, latestEN]
