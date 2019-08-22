{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module Server where

import           Control.Applicative    (optional)
import Control.Monad (msum)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (toLower)
import qualified Data.Text              as T
import           Happstack.Server       hiding (Session)
import Data.Maybe (maybe)

import Blog
import BlogStore
import Locales
import RSS

instance FromReqURI BlogLang where
  fromReqURI sub =
    case map toLower sub of
      "de" -> Just DE
      "en" -> Just EN
      _    -> Nothing

pageSize :: Int
pageSize = 3

tmpPolicy :: BodyPolicy
tmpPolicy = defaultBodyPolicy "/tmp" 0 200000 1000

runBlog :: Int -> String -> IO ()
runBlog port respath = do
  withCache "blog.tazj.in." $ \cache ->
    simpleHTTP nullConf {port = port} $ tazBlog cache respath

tazBlog :: BlogCache -> String -> ServerPart Response
tazBlog cache resDir = do
    msum [ path $ \(lang :: BlogLang) -> blogHandler cache lang
         , dir "static" $ staticHandler resDir
         , blogHandler cache EN
         , staticHandler resDir
         , notFound $ toResponse $ showError NotFound DE
         ]

blogHandler :: BlogCache -> BlogLang -> ServerPart Response
blogHandler cache lang =
    msum [ path $ \(eId :: Integer) -> showEntry cache lang $ EntryId eId
         , nullDir >> showIndex cache lang
         , dir "rss" $ nullDir >> showRSS cache lang
         , dir "rss.xml" $ nullDir >> showRSS cache lang
         , notFound $ toResponse $ showError NotFound lang
         ]

staticHandler :: String -> ServerPart Response
staticHandler resDir = do
  setHeaderM "cache-control" "max-age=630720000"
  setHeaderM "expires" "Tue, 20 Jan 2037 04:20:42 GMT"
  serveDirectory DisableBrowsing [] resDir

showEntry :: BlogCache -> BlogLang -> EntryId -> ServerPart Response
showEntry cache lang eId = do
    entry <- getEntry cache eId
    tryEntry entry lang

tryEntry :: Maybe Entry -> BlogLang -> ServerPart Response
tryEntry Nothing lang = notFound $ toResponse $ showError NotFound lang
tryEntry (Just entry) _ = ok $ toResponse $ blogTemplate eLang eTitle $ renderEntry entry
    where
        eTitle = T.append ": " (title entry)
        eLang = lang entry

offset :: Maybe Int -> Int
offset = maybe 0 ((*) pageSize)

showIndex :: BlogCache -> BlogLang -> ServerPart Response
showIndex cache lang = do
    (page :: Maybe Int) <- optional $ lookRead "page"
    entries <- listEntries cache (offset page) pageSize
    ok $ toResponse $ blogTemplate lang "" $
        renderEntries entries (Just $ showLinks page lang)

showRSS :: BlogCache -> BlogLang -> ServerPart Response
showRSS cache lang = do
    entries <- listEntries cache 0 4
    feed <- liftIO $ renderFeed lang entries
    setHeaderM "content-type" "text/xml"
    ok $ toResponse feed
