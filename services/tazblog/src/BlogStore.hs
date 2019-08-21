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
module BlogStore(
  BlogCache,
  EntryId(..),
  Entry(..),
  withCache,
  listEntries,
  getEntry,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Locales (BlogLang (..))
import Network.DNS.Lookup (lookupTXT)
import qualified Network.DNS.Resolver as R

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

-- | Wraps a DNS resolver with caching configured. For the initial
-- version of this, all caching of entries is done by the resolver
-- (i.e. no pre-assembled versions of entries are cached).
data BlogCache
  = BlogCache { resolver :: R.Resolver
              , zone :: String }

type Offset = Integer

type Count = Integer

withCache :: String -> (BlogCache -> IO a) -> IO a
withCache zone f = do
  let conf = R.defaultResolvConf { R.resolvCache = Just R.defaultCacheConf
                                 , R.resolvConcurrent = True }
  seed <- R.makeResolvSeed conf
  R.withResolver seed $ (\r -> f $ BlogCache r zone)

listEntries :: MonadIO m => BlogCache -> Offset -> Count -> m [Entry]
listEntries (BlogCache r z) offset count = undefined

getEntry :: MonadIO m => BlogCache -> EntryId -> m (Maybe Entry)
getEntry (BlogCache r z) eId = undefined
