{-# LANGUAGE RecordWildCards #-}

module RSS (renderFeed) where

import qualified Data.Text   as T

import           Data.Maybe  (fromMaybe)
import           Data.Time   (UTCTime, getCurrentTime)
import           Network.URI
import           Text.RSS

import           BlogDB      hiding (Title)
import           Locales

createChannel :: BlogLang -> UTCTime -> [ChannelElem]
createChannel l  now = [ Language $ show l
                       , Copyright "Vincent Ambo"
                       , WebMaster "tazjin@googlemail.com"
                       , ChannelPubDate now
                       ]

createRSS :: BlogLang -> UTCTime -> [Item] -> RSS
createRSS l t i = RSS (rssTitle l) (rssLink l) (rssDesc l) (createChannel l t) i

createItem :: Entry -> Item
createItem Entry{..} = [ Title $ T.unpack title
                       , Link $ makeLink lang entryId
                       , Description $ T.unpack btext
                       , PubDate edate]

makeLink :: BlogLang -> EntryId -> URI
makeLink l i = let url = "http://tazj.in/" ++ show l ++ "/" ++ show i
               in fromMaybe nullURI $ parseURI url

createItems :: [Entry] -> [Item]
createItems = map createItem

createFeed :: BlogLang -> [Entry] -> IO RSS
createFeed l e = getCurrentTime >>= (\t -> return $ createRSS l t $ createItems e )

renderFeed :: BlogLang -> [Entry] -> IO String
renderFeed l e = createFeed l e >>= (\feed -> return $ showXML $ rssToXML feed)
