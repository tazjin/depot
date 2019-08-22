{-# LANGUAGE RecordWildCards #-}
module RSS (renderFeed) where

import qualified Data.Text as T

import Control.Monad (liftM)
import Data.Maybe    (fromMaybe)
import Data.Time     (UTCTime(..), getCurrentTime, secondsToDiffTime)
import Network.URI
import Text.RSS

import BlogStore
import Locales

createChannel :: BlogLang -> UTCTime -> [ChannelElem]
createChannel l now = [ Language $ show l
                      , Copyright "Vincent Ambo"
                      , WebMaster "tazjin@gmail.com"
                      , ChannelPubDate now
                      ]

createRSS :: BlogLang -> UTCTime -> [Item] -> RSS
createRSS l t = RSS (rssTitle l) (rssLink l) (rssDesc l) (createChannel l t)

createItem :: Entry -> Item
createItem Entry{..} = [ Title $ T.unpack title
                       , Link $ makeLink lang entryId
                       , Description $ T.unpack btext
                       , PubDate $ UTCTime edate $ secondsToDiffTime 0 ]

makeLink :: BlogLang -> EntryId -> URI
makeLink l i = let url = "http://tazj.in/" ++ show l ++ "/" ++ show i
               in fromMaybe nullURI $ parseURI url

createItems :: [Entry] -> [Item]
createItems = map createItem

createFeed :: BlogLang -> [Entry] -> IO RSS
createFeed l e = getCurrentTime >>= (\t -> return $ createRSS l t $ createItems e )

renderFeed :: BlogLang -> [Entry] -> IO String
renderFeed l e = liftM (showXML . rssToXML) (createFeed l e)
