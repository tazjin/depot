module BlogDB where

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Remote
import Data.ByteString      (ByteString)
import Data.Data            (Data, Typeable)
import Data.IxSet           (Indexable (..), IxSet, Proxy (..), getOne, ixFun, ixSet, (@=))
import Data.SafeCopy        (base, deriveSafeCopy)
import Data.Text            (Text, pack)
import Data.Time
import Network              (PortID (..))
import System.Environment   (getEnv)

import qualified Crypto.Hash.SHA512     as SHA (hash)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString.Char8  as B
import qualified Data.IxSet             as IxSet

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

$(deriveSafeCopy 0 'base ''BlogLang)

data Entry = Entry {
    entryId :: EntryId,
    lang    :: BlogLang,
    author  :: Text,
    title   :: Text,
    btext   :: Text,
    mtext   :: Text,
    edate   :: UTCTime
} deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 2 'base ''Entry)

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

$(deriveSafeCopy 2 'base ''Author)
$(deriveSafeCopy 2 'base ''Title)
$(deriveSafeCopy 2 'base ''BText)
$(deriveSafeCopy 2 'base ''MText)
$(deriveSafeCopy 2 'base ''Tag)
$(deriveSafeCopy 2 'base ''EDate)
$(deriveSafeCopy 2 'base ''SDate)
$(deriveSafeCopy 2 'base ''Username)
$(deriveSafeCopy 2 'base ''SessionID)

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

updateEntry :: Entry -> Update Blog Entry
updateEntry e =
    do b@Blog{..} <- get
       put $ b { blogEntries = IxSet.updateIx (entryId e) e blogEntries }
       return e

deleteEntry :: EntryId -> Update Blog EntryId
deleteEntry entry =
    do b@Blog{..} <- get
       put $ b { blogEntries = IxSet.deleteIx entry blogEntries }
       return entry

getEntry :: EntryId -> Query Blog (Maybe Entry)
getEntry eId =
    do Blog{..} <- ask
       return $ getOne $ blogEntries @= eId

latestEntries :: BlogLang -> Query Blog [Entry]
latestEntries lang =
    do Blog{..} <- ask
       return $ IxSet.toDescList (Proxy :: Proxy EDate) $ blogEntries @= lang

addSession :: Session -> Update Blog Session
addSession nSession =
    do b@Blog{..} <- get
       put $ b { blogSessions = IxSet.insert nSession blogSessions}
       return nSession

getSession :: SessionID -> Query Blog (Maybe Session)
getSession sId =
  do Blog{..} <- ask
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
  do Blog{..} <- ask
     return $ getOne $ blogUsers @= uN

checkUser :: Username -> String -> Query Blog Bool
checkUser uN pw =
  do Blog{..} <- ask
     let user = getOne $ blogUsers @= uN
     case user of
       Nothing  -> return False
       (Just u) -> return $ password u == hashString pw

-- various functions
hashString :: String -> ByteString
hashString = B64.encode .  SHA.hash . B.pack

$(makeAcidic ''Blog
    [ 'insertEntry
    , 'updateEntry
    , 'deleteEntry
    , 'getEntry
    , 'latestEntries
    , 'addSession
    , 'getSession
    , 'addUser
    , 'getUser
    , 'checkUser
    , 'clearSessions
    ])

interactiveUserAdd :: String -> IO ()
interactiveUserAdd dbHost = do
  acid <- openRemoteState skipAuthenticationPerform dbHost (PortNumber 8070)
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
