{-# LANGUAGE RecordWildCards #-}

module RSS
  ( renderFeed
    )
where

import BlogStore
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime (..), getCurrentTime, secondsToDiffTime)
import Locales
import Network.URI
import Text.RSS

createChannel :: BlogLang -> UTCTime -> [ChannelElem]
createChannel l now =
  [ Language $ show l,
    Copyright "Vincent Ambo",
    WebMaster "mail@tazj.in",
    ChannelPubDate now
    ]

createRSS :: BlogLang -> UTCTime -> [Item] -> RSS
createRSS l t = RSS (rssTitle l) (rssLink l) (rssDesc l) (createChannel l t)

createItem :: Entry -> Item
createItem Entry {..} =
  [ Title $ T.unpack title,
    Link $ makeLink lang entryId,
    Description $ T.unpack text,
    PubDate $ UTCTime edate $ secondsToDiffTime 0
    ]

makeLink :: BlogLang -> EntryId -> URI
makeLink l i =
  let url = "http://tazj.in/" ++ show l ++ "/" ++ show i
   in fromMaybe nullURI $ parseURI url

createItems :: [Entry] -> [Item]
createItems = map createItem

createFeed :: BlogLang -> [Entry] -> IO RSS
createFeed l e = getCurrentTime >>= (\t -> return $ createRSS l t $ createItems e)

renderFeed :: BlogLang -> [Entry] -> IO String
renderFeed l e = liftM (showXML . rssToXML) (createFeed l e)
