{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Blog
import BlogStore
import Control.Applicative (optional)
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (maybe)
import qualified Data.Text as T
import Happstack.Server hiding (Session)
import RSS

pageSize :: Int
pageSize = 3

tmpPolicy :: BodyPolicy
tmpPolicy = defaultBodyPolicy "/tmp" 0 200000 1000

runBlog :: Int -> String -> IO ()
runBlog port respath =
  withCache "blog.tazj.in." $ \cache ->
    simpleHTTP nullConf {port = port} $ tazblog cache respath

tazblog :: BlogCache -> String -> ServerPart Response
tazblog cache resDir =
  msum
    [ -- legacy language-specific routes
      dir "de" $ blogHandler cache,
      dir "en" $ blogHandler cache,
      dir "static" $ staticHandler resDir,
      blogHandler cache,
      staticHandler resDir,
      notFound $ toResponse $ showError "Not found" "Page not found"
      ]

blogHandler :: BlogCache -> ServerPart Response
blogHandler cache =
  msum
    [ path $ \(eId :: Integer) -> showEntry cache $ EntryId eId,
      nullDir >> showIndex cache,
      dir "rss" $ nullDir >> showRSS cache,
      dir "rss.xml" $ nullDir >> showRSS cache
      ]

staticHandler :: String -> ServerPart Response
staticHandler resDir = do
  setHeaderM "cache-control" "max-age=630720000"
  setHeaderM "expires" "Tue, 20 Jan 2037 04:20:42 GMT"
  serveDirectory DisableBrowsing [] resDir

showEntry :: BlogCache -> EntryId -> ServerPart Response
showEntry cache eId = do
  entry <- getEntry cache eId
  tryEntry entry

tryEntry :: Maybe Entry -> ServerPart Response
tryEntry Nothing = notFound $ toResponse $ showError "Not found" "Blog entry not found"
tryEntry (Just entry) = ok $ toResponse $ blogTemplate eTitle $ renderEntry entry
  where
    eTitle = T.append ": " (title entry)

offset :: Maybe Int -> Int
offset = maybe 0 (pageSize *)

showIndex :: BlogCache -> ServerPart Response
showIndex cache = do
  (page :: Maybe Int) <- optional $ lookRead "page"
  entries <- listEntries cache (offset page) pageSize
  ok $ toResponse $ blogTemplate ""
    $ renderEntries entries (Just $ showLinks page)

showRSS :: BlogCache -> ServerPart Response
showRSS cache = do
  entries <- listEntries cache 0 4
  feed <- liftIO $ renderFeed entries
  setHeaderM "content-type" "text/xml"
  ok $ toResponse feed
