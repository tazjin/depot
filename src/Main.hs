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
         , path $ \(year :: Int ) -> path $ \(month :: Int) -> showMonth year month lang
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
tryEntry (Just entry) = toResponse $ blogTemplate eLang eTitle $ renderEntry entry
    where
        eTitle = ": " ++ title entry
        eLang = lang entry

showIndex :: BlogLang -> ServerPart Response
showIndex lang = do
    entries <- getLatest lang [("limit", toJSON (7 :: Int)), ("descending", toJSON True)]
    ok $ toResponse $ blogTemplate lang "" $ renderEntries entries (topText lang)

showMonth :: Int -> Int -> BlogLang -> ServerPart Response
showMonth y m lang = do
    entries <- getLatest lang $ makeQuery startkey endkey
    ok $ toResponse $ blogTemplate lang month 
        $ renderEntries entries month
  where
    month = getMonth lang y  m
    startkey = JSArray [toJSON y, toJSON m]
    endkey = JSArray [toJSON y, toJSON m, JSObject (toJSObject [] )]

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
queryDB view arg = liftIO . runCouchDB' $ queryView (db "tazblog") (doc "entries") (doc view) arg

maybeDoc :: Data a => Maybe (Doc, Rev, JSValue) -> Maybe a
maybeDoc (Just(_,_,v)) = Just( stripResult $ fromJSON v)
maybeDoc Nothing = Nothing

stripResult :: Result a -> a
stripResult (Ok z) = z
stripResult (Error s) = error $ "JSON error: " ++ s

getMonthCount :: BlogLang -> Int -> Int -> ServerPart Int
getMonthCount lang y m  = do
    count <- queryDB (view lang) $ makeQuery startkey endkey
    return . stripCount $ map (stripResult . fromJSON . snd) count
  where
    startkey = JSArray [toJSON ("count" :: String), toJSON y, toJSON m]
    endkey = JSArray [toJSON ("count" :: String), toJSON y, toJSON m, JSObject (toJSObject [] )]
    stripCount :: [Int] -> Int
    stripCount [x] = x
    stripCount [] = 0
    view DE = "countDE"
    view EN = "countEN"


-- CouchDB View Setup
latestDEView = "function(doc){ if(doc.lang == 'DE'){ emit([doc.year, doc.month, doc.day, doc._id], doc); } }"
latestENView = "function(doc){ if(doc.lang == 'EN'){ emit([doc.year, doc.month, doc.day, doc._id], doc); } }"
countDEView  = "function(doc){ if(doc.lang == 'DE'){ emit(['count', doc.year, doc.month, doc.day, doc._id], 1); } }"
countENView  = "function(doc){ if(doc.lang == 'EN'){ emit(['count', doc.year, doc.month, doc.day, doc._id], 1); } }"
countReduce  = "function(keys, values, rereduce) { return sum(values); }"

latestDE = ViewMap "latestDE" latestDEView
latestEN = ViewMap "latestEN" latestENView
countDE  = ViewMapReduce "countDE" countDEView countReduce
countEN  = ViewMapReduce "countEN" countENView countReduce

setupBlogViews :: IO ()
setupBlogViews = runCouchDB' $ 
    newView "tazblog" "entries" [latestDE, latestEN, countDE, countEN]
