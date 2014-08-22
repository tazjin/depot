{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Control.Monad.Reader   (ask)
import           Control.Monad.State    (get, put)
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Acid.Local
import           Data.ByteString        (ByteString)
import           Data.Char              (toLower)
import           Data.Data              (Data, Typeable)
import           Data.IxSet             (Indexable (..), IxSet (..), Proxy (..),
                                         getOne, ixFun, ixSet, (@=))
import           Data.List              (insert)
import           Data.SafeCopy          
import           Data.Text              (Text, pack)
import           Data.Text.Lazy         (toStrict)
import           Data.Time
import           Happstack.Server       (FromReqURI (..))
import           System.Environment     (getEnv)

import qualified Crypto.Hash.SHA512     as SHA (hash)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString.Char8  as B
import qualified Data.IxSet             as IxSet
import qualified Data.Text              as Text
             
newtype EntryId = EntryId { unEntryId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable)

$(deriveSafeCopy 2 'base ''EntryId)
  
instance Show EntryId where
  show = show . unEntryId

data BlogLang = EN | DE
    deriving (Eq, Ord, Data, Typeable)

instance Show BlogLang where
    show DE = "de"
    show EN = "en"

instance FromReqURI BlogLang where
  fromReqURI sub =
    case map toLower sub of
      "de" -> Just DE
      "en" -> Just EN
      _    -> Nothing

$(deriveSafeCopy 0 'base ''BlogLang)

data Comment = Comment {
    cdate   :: UTCTime,
    cauthor :: Text,
    ctext   :: Text
} deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Comment)

data Entry_v0 = Entry_v0 {
    entryId_v0  :: EntryId,
    lang_v0     :: BlogLang,
    author_v0   :: Text,
    title_v0    :: Text,
    btext_v0    :: Text,
    mtext_v0    :: Text,
    edate_v0    :: UTCTime,
    tags     :: [Text],
    comments :: [Comment]
} deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Entry_v0)

data Entry = Entry {
    entryId  :: EntryId,
    lang     :: BlogLang,
    author   :: Text,
    title    :: Text,
    btext    :: Text,
    mtext    :: Text,
    edate    :: UTCTime
} deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 2 'extension ''Entry)

instance Migrate Entry where
  type MigrateFrom Entry = Entry_v0
  migrate (Entry_v0 ei l a t b m ed _ _) =
    Entry ei l a t b m ed
  
-- ixSet requires different datatypes for field indexes, so let's define some
newtype Author_v0 = Author_v0 Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Author = Author Text   deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 2 'extension ''Author)
instance Migrate Author where
  type MigrateFrom Author = Author_v0
  migrate (Author_v0 x) = Author x

newtype Title_v0  = Title_v0 Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Title  = Title Text    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 2 'extension ''Title)
instance Migrate Title where
  type MigrateFrom Title = Title_v0
  migrate (Title_v0 x) = Title x

newtype BText_v0  = BText_v0 Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype BText  = BText Text    deriving (Eq, Ord, Data, Typeable) -- standard text
$(deriveSafeCopy 2 'extension ''BText)
instance Migrate BText where
  type MigrateFrom BText = BText_v0
  migrate (BText_v0 x) = BText x

newtype MText_v0  = MText_v0 Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype MText  = MText Text    deriving (Eq, Ord, Data, Typeable) -- "read more" text
$(deriveSafeCopy 2 'extension ''MText)
instance Migrate MText where
  type MigrateFrom MText = MText_v0
  migrate (MText_v0 x) = MText x

newtype Tag_v0    = Tag_v0 Text      deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Tag    = Tag Text      deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 2 'extension ''Tag)
instance Migrate Tag where
  type MigrateFrom Tag = Tag_v0
  migrate (Tag_v0 x) = Tag x

newtype EDate_v0  = EDate_v0 UTCTime deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype EDate  = EDate UTCTime deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 2 'extension ''EDate)
instance Migrate EDate where
  type MigrateFrom EDate = EDate_v0
  migrate (EDate_v0 x) = EDate x

newtype SDate_v0  = SDate_v0 UTCTime   deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype SDate  = SDate UTCTime   deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 2 'extension ''SDate)
instance Migrate SDate where
  type MigrateFrom SDate = SDate_v0
  migrate (SDate_v0 x) = SDate x

newtype Username_v0 = Username_v0 Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Username = Username Text deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 2 'extension ''Username)
instance Migrate Username where
  type MigrateFrom Username = Username_v0
  migrate (Username_v0 x) = Username x
  
newtype SessionID_v0 = SessionID_v0 Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype SessionID = SessionID Text deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 2 'extension ''SessionID)
instance Migrate SessionID where
  type MigrateFrom SessionID = SessionID_v0
  migrate (SessionID_v0 x) = SessionID x

instance Indexable Entry where
    empty = ixSet [ ixFun $ \e -> [ entryId e]
                  , ixFun $ (:[]) . lang
                  , ixFun $ \e -> [ Author $ author e ]
                  , ixFun $ \e -> [ Title $ title e]
                  , ixFun $ \e -> [ BText $ btext e]
                  , ixFun $ \e -> [ MText $ mtext e]
                  , ixFun $ \e -> [ EDate $ edate e]
                  ]

data User = User {
    username :: Text,
    password :: ByteString
} deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''User)

data Session = Session {
    sessionID :: Text,
    user      :: User,
    sdate     :: UTCTime
} deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Session)

instance Indexable User where
    empty = ixSet [ ixFun $ \u -> [Username $ username u]
                  , ixFun $ (:[]) . password
                  ]

instance Indexable Session where
    empty = ixSet [ ixFun $ \s -> [SessionID $ sessionID s]
                  , ixFun $ (:[]) . user
                  , ixFun $ \s -> [SDate $ sdate s]
                  ]

data Blog = Blog {
    blogSessions :: IxSet Session,
    blogUsers    :: IxSet User,
    blogEntries  :: IxSet Entry
} deriving (Data, Typeable)

latestEntries :: BlogLang -> Query Blog [Entry]
latestEntries lang =
    do b@Blog{..} <- ask
       return $ IxSet.toDescList (Proxy :: Proxy EDate) $ blogEntries @= lang

$(deriveSafeCopy 0 'base ''Blog)
                         
$(makeAcidic ''Blog ['latestEntries])

initialBlogState :: Blog
initialBlogState =
    Blog { blogSessions = empty
         , blogUsers = empty
         , blogEntries = empty }

main :: IO ()
main = do
  putStrLn "Opening state"
  acid <- openLocalStateFrom "/var/tazblog/BlogState" initialBlogState
  entries <- query acid (LatestEntries EN)
  print $ length entries
  print $ head entries
  putStrLn "Creating checkpoint"
  createCheckpoint acid
  putStrLn "Closing state"
  closeAcidState acid
