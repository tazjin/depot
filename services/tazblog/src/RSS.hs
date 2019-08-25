{-# LANGUAGE RecordWildCards #-}

module RSS
  ( renderFeed
    )
where

import BlogStore
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time (UTCTime (..), getCurrentTime, secondsToDiffTime)
import Network.URI (URI, parseURI)
import Text.RSS

createChannel :: UTCTime -> [ChannelElem]
createChannel now =
  [ Language "en",
    Copyright "Vincent Ambo",
    WebMaster "mail@tazj.in",
    ChannelPubDate now
    ]

createRSS :: UTCTime -> [Item] -> RSS
createRSS t =
  let link = fromJust $ parseURI "https://tazj.in"
   in RSS "tazjin's blog" link "tazjin's blog feed" (createChannel t)

createItem :: Entry -> Item
createItem Entry {..} =
  [ Title "tazjin's blog",
    Link $ entryLink entryId,
    Description $ T.unpack text,
    PubDate $ UTCTime edate $ secondsToDiffTime 0
    ]

entryLink :: EntryId -> URI
entryLink i =
  let url = "http://tazj.in/" ++ "/" ++ show i
   in fromJust $ parseURI url

createItems :: [Entry] -> [Item]
createItems = map createItem

createFeed :: [Entry] -> IO RSS
createFeed e = getCurrentTime >>= (\t -> return $ createRSS t $ createItems e)

renderFeed :: [Entry] -> IO String
renderFeed e = liftM (showXML . rssToXML) (createFeed e)
