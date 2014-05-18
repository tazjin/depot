module BlogDB where

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
import           Data.SafeCopy          (SafeCopy, base, deriveSafeCopy)
import           Data.Text              (Text, pack)
import           Data.Text.Lazy         (toStrict)
import           Data.Time
import           System.Environment     (getEnv)

import qualified Crypto.Hash.SHA512     as SHA (hash)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString.Char8  as B
import qualified Data.IxSet             as IxSet
import qualified Data.Text              as Text


newtype EntryId = EntryId { unEntryId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable)

$(deriveSafeCopy 0 'base ''EntryId)

instance Show EntryId where
  show = show . unEntryId

data BlogLang = EN | DE
    deriving (Eq, Ord, Data, Typeable)

instance Show BlogLang where
    show DE = "de"
    show EN = "en"

$(deriveSafeCopy 0 'base ''BlogLang)

data Comment = Comment {
    cdate   :: UTCTime,
    cauthor :: Text,
    ctext   :: Text
} deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Comment)

data Entry = Entry {
    entryId  :: EntryId,
    lang     :: BlogLang,
    author   :: Text,
    title    :: Text,
    btext    :: Text,
    mtext    :: Text,
    edate    :: UTCTime,
    tags     :: [Text],
    comments :: [Comment]
} deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Entry)

-- ixSet requires different datatypes for field indexes, so let's define some
newtype Author = Author Text   deriving (Eq, Ord, Data, Typeable)
newtype Title  = Title Text    deriving (Eq, Ord, Data, Typeable)
newtype BText  = BText Text    deriving (Eq, Ord, Data, Typeable) -- standard text
newtype MText  = MText Text    deriving (Eq, Ord, Data, Typeable) -- "read more" text
newtype Tag    = Tag Text      deriving (Eq, Ord, Data, Typeable)
newtype EDate  = EDate UTCTime deriving (Eq, Ord, Data, Typeable)
newtype SDate  = SDate UTCTime   deriving (Eq, Ord, Data, Typeable)
newtype Username = Username Text deriving (Eq, Ord, Data, Typeable)
newtype SessionID = SessionID Text deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Author)
$(deriveSafeCopy 0 'base ''Title)
$(deriveSafeCopy 0 'base ''BText)
$(deriveSafeCopy 0 'base ''MText)
$(deriveSafeCopy 0 'base ''Tag)
$(deriveSafeCopy 0 'base ''EDate)
$(deriveSafeCopy 0 'base ''SDate)
$(deriveSafeCopy 0 'base ''Username)
$(deriveSafeCopy 0 'base ''SessionID)

instance Indexable Entry where
    empty = ixSet [ ixFun $ \e -> [ entryId e]
                  , ixFun $ (:[]) . lang
                  , ixFun $ \e -> [ Author $ author e ]
                  , ixFun $ \e -> [ Title $ title e]
                  , ixFun $ \e -> [ BText $ btext e]
                  , ixFun $ \e -> [ MText $ mtext e]
                  , ixFun $ \e -> [ EDate $ edate e]
                  , ixFun $ \e -> map Tag (tags e)
                  , ixFun comments
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

$(deriveSafeCopy 0 'base ''Blog)

initialBlogState :: Blog
initialBlogState =
    Blog { blogSessions = empty
         , blogUsers = empty
         , blogEntries = empty }

-- acid-state database functions (purity is necessary!)

insertEntry :: Entry -> Update Blog Entry
insertEntry e =
    do b@Blog{..} <- get
       put $ b { blogEntries = IxSet.insert e blogEntries }
       return e

addComment :: EntryId -> Comment -> Update Blog Entry
addComment eId c =
	do b@Blog{..} <- get
	   let (Just e) = getOne $ blogEntries @= eId
	   let newEntry = e { comments = insert c $ comments e }
	   put $ b { blogEntries = IxSet.updateIx eId newEntry blogEntries }
	   return newEntry

deleteComment :: EntryId -> UTCTime -> Update Blog Entry
deleteComment eId cDate =
   do b@Blog{..} <- get
      let (Just e) = getOne $ blogEntries @= eId
      let newEntry = e {comments = filter (\c -> cdate c /= cDate) (comments e)}
      put $ b { blogEntries = IxSet.updateIx eId newEntry blogEntries }
      return newEntry

updateEntry :: Entry -> Update Blog Entry
updateEntry e =
    do b@Blog{..} <- get
       put $ b { blogEntries = IxSet.updateIx (entryId e) e blogEntries}
       return e

getEntry :: EntryId -> Query Blog (Maybe Entry)
getEntry eId =
    do b@Blog{..} <- ask
       return $ getOne $ blogEntries @= eId

latestEntries :: BlogLang -> Query Blog [Entry]
latestEntries lang =
    do b@Blog{..} <- ask
       return $ IxSet.toDescList (Proxy :: Proxy EDate) $ blogEntries @= lang

addSession :: Session -> Update Blog Session
addSession nSession =
    do b@Blog{..} <- get
       put $ b { blogSessions = IxSet.insert nSession blogSessions}
       return nSession

getSession :: SessionID -> Query Blog (Maybe Session)
getSession sId =
  do b@Blog{..} <- ask
     return $ getOne $ blogSessions @= sId

clearSessions :: Update Blog [Session]
clearSessions =
  do b@Blog{..} <- get
     put $ b { blogSessions = empty }
     return []

addUser :: Text -> String -> Update Blog User
addUser un pw =
    do b@Blog{..} <- get
       let u = User un $ hashString pw
       put $ b { blogUsers = IxSet.insert u blogUsers}
       return u

getUser :: Username -> Query Blog (Maybe User)
getUser uN =
  do b@Blog{..} <- ask
     return $ getOne $ blogUsers @= uN

checkUser :: Username -> String -> Query Blog Bool
checkUser uN pw =
  do b@Blog{..} <- ask
     let user = getOne $ blogUsers @= uN
     case user of
       Nothing  -> return False
       (Just u) -> return $ password u == hashString pw

-- various functions
hashString :: String -> ByteString
hashString = B64.encode .  SHA.hash . B.pack

$(makeAcidic ''Blog
    [ 'insertEntry
    , 'addComment
    , 'deleteComment
    , 'updateEntry
    , 'getEntry
    , 'latestEntries
    , 'addSession
    , 'getSession
    , 'addUser
    , 'getUser
    , 'checkUser
    , 'clearSessions
    ])

interactiveUserAdd :: IO ()
interactiveUserAdd = do
  tbDir <- getEnv "TAZBLOG"
  acid <- openLocalStateFrom (tbDir ++ "/BlogState") initialBlogState
  putStrLn "Username:"
  un <- getLine
  putStrLn "Password:"
  pw <- getLine
  update' acid (AddUser (pack un) pw)
  closeAcidState acid

flushSessions :: IO ()
flushSessions = do
  tbDir <- getEnv "TAZBLOG"
  acid <- openLocalStateFrom (tbDir ++ "/BlogState") initialBlogState
  update' acid ClearSessions
  closeAcidState acid

archiveState :: IO ()
archiveState = do
    tbDir <- getEnv "TAZBLOG"
    acid <- openLocalStateFrom (tbDir ++ "/BlogState") initialBlogState
    createArchive acid
    closeAcidState acid
