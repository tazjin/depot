-- |This module implements fetching of individual blog entries from
-- DNS. Yes, you read that correctly.
--
-- Each blog post is stored as a set of records in a designated DNS
-- zone. For the production blog, this zone is `blog.tazj.in.`.
--
-- A top-level record at `_posts` contains a list of all published
-- post IDs.
--
-- For each of these post IDs, there is a record at `_meta.$postID`
-- that contains the title and number of post chunks.
--
-- For each post chunk, there is a record at `_$chunkID.$postID` that
-- contains a base64-encoded post fragment.
--
-- This module implements logic for assembling a post out of these
-- fragments and caching it based on the TTL of its `_meta` record.
module BlogStore where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Locales (BlogLang (..))

newtype EntryId = EntryId {unEntryId :: Integer}
  deriving (Eq, Ord)

instance Show EntryId where

  show = show . unEntryId

data Entry
  = Entry
      { entryId :: EntryId,
        lang :: BlogLang,
        author :: Text,
        title :: Text,
        btext :: Text,
        mtext :: Text,
        edate :: UTCTime
        }
  deriving (Eq, Ord, Show)

data BlogCache

type Offset = Integer

type Count = Integer

newCache :: String -> IO BlogCache
newCache zone = undefined

listEntries :: MonadIO m => BlogCache -> Offset -> Count -> m [Entry]
listEntries cache offset count = undefined

getEntry :: MonadIO m => BlogCache -> EntryId -> m (Maybe Entry)
getEntry cache eId = undefined
