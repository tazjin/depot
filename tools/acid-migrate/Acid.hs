{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, 
TemplateHaskell, TypeFamilies, OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module Main where
import Control.Applicative  ((<$>), optional)
import Control.Exception    (bracket)
import Control.Monad        (msum, mzero)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (liftIO)
import Data.Acid
import Data.Acid.Advanced 
import Data.Acid.Local
import Data.ByteString      (ByteString)
import Data.Data            (Data, Typeable)
import Data.IxSet           (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet)
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Text            (Text, pack)
import Data.Text.Lazy       (toStrict)
import Data.Time
import           System.Environment(getEnv)


import qualified Crypto.Hash.SHA512 as SHA (hash)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.IxSet as IxSet
import qualified Data.Text  as Text


{-CouchDB imports-}

import Database.CouchDB hiding (runCouchDB')
import Database.CouchDB.JSON
import Text.JSON
import Data.List (intersperse, (\\))
import System.Locale (defaultTimeLocale)

-- data types and acid-state setup

newtype EntryId = EntryId { unEntryId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)

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
    entryId :: EntryId,
    lang   :: BlogLang,
    author :: Text,
    title  :: Text,
    btext  :: Text, 
    mtext  :: Text,
    edate  :: UTCTime,
    tags   :: [Text],
    comments :: [Comment]
} deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Entry)

-- ixSet requires different datatypes for field indexes, so let's define some
newtype Author = Author Text   deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Title  = Title Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype BText  = BText Text    deriving (Eq, Ord, Data, Typeable, SafeCopy) -- standard text
newtype MText  = MText Text    deriving (Eq, Ord, Data, Typeable, SafeCopy) -- "read more" text
newtype Tag    = Tag Text      deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype EDate  = EDate UTCTime deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype SDate  = SDate UTCTime   deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Username = Username Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype SessionID = SessionID Text deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable Entry where 
    empty = ixSet [ ixFun $ \e -> [ entryId e]
                  , ixFun $ (:[]) . lang
                  , ixFun $ \e -> [ Author $ author e ]
                  , ixFun $ \e -> [ Title $ title e]
                  , ixFun $ \e -> [ BText $ btext e]
                  , ixFun $ \e -> [ MText $ mtext e]
                  , ixFun $ \e -> [ EDate $ edate e]
                  , ixFun $ \e -> map Tag (tags e)
                  , ixFun $ comments
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
       put $ b { blogEntries = IxSet.updateIx (entryId e) e blogEntries}
       return e

getPost :: EntryId -> Query Blog (Maybe Entry)
getPost eid =
    do b@Blog{..} <- ask
       return $ getOne $ blogEntries @= eid

latestPosts :: Query Blog [Entry]
latestPosts =
    do b@Blog{..} <- ask
       return $ IxSet.toDescList (Proxy :: Proxy EDate) $ blogEntries

addSession :: Text -> User -> UTCTime -> Update Blog Session
addSession sId u t =
    do b@Blog{..} <- get
       let s = Session sId u t
       put $ b { blogSessions = IxSet.insert s blogSessions}
       return s

addUser :: Text -> String -> Update Blog User
addUser un pw =
    do b@Blog{..} <- get
       let u = User un $ hashString pw
       put $ b { blogUsers = IxSet.insert u blogUsers}
       return u

-- various functions
hashString :: String -> ByteString
hashString = B64.encode .  SHA.hash . B.pack

$(makeAcidic ''Blog
    [ 'insertEntry
    , 'updateEntry
    , 'getPost
    , 'latestPosts
    , 'addSession
    , 'addUser
    ])

-- CouchDB database functions

runCouchDB' :: CouchMonad a -> IO a
runCouchDB' = runCouchDB "127.0.0.1" 5984

instance JSON Comment where
    showJSON = undefined
    readJSON val = do
        obj <- jsonObject val
        scauthor <- jsonField "cauthor" obj
        jsscdate <- jsonField "cdate" obj :: Result JSValue
        let rcdate = stripResult $ jsonInt jsscdate
        sctext <- jsonField "ctext" obj
        return $ Comment (parseSeconds rcdate) (pack scauthor) (pack sctext)

instance JSON Entry where
    showJSON = undefined
    readJSON val = do
        obj <- jsonObject val
        sauthor <- jsonField "author" obj
        stitle <- jsonField "title" obj
        day <- jsonField "day" obj
        month <- jsonField "month" obj
        year <- jsonField "year" obj
        stext <- jsonField "text" obj
        comments <- jsonField "comments" obj
        oldid <- jsonField "_id" obj
        let leTime = parseShittyTime year month day oldid
        return $ Entry (EntryId $ getUnixTime leTime) DE (pack sauthor) (pack $ stitle \\ "\n") (pack stext) (Text.empty) 
                        leTime [] comments


getUnixTime :: UTCTime -> Integer
getUnixTime t = read $ formatTime defaultTimeLocale "%s" t

parseSeconds :: Integer -> UTCTime
parseSeconds t = readTime defaultTimeLocale "%s" $ show t

parseShittyTime :: Int -> Int -> Int -> String -> UTCTime
parseShittyTime y m d i = readTime defaultTimeLocale "%Y %m %e  %k:%M:%S" newPartTime
    where
        firstPart = take 2 i
        secondPart = take 2 $ drop 2 i
        thirdPart = drop 4 i
        newPartTime =  concat $ intersperse " " [show y, showMonth m, show d, " "] ++ 
                        intersperse ":" [firstPart, secondPart, thirdPart]
        showMonth mn  
                | mn < 10 = "0" ++ show mn
                | otherwise = show mn

getOldEntries = runCouchDB' $ queryView (db "tazblog") (doc "entries") (doc "latestDE") []

parseOldEntries :: IO [Entry]
parseOldEntries = do
    queryResult <- getOldEntries
    let entries = map (stripResult . readJSON . snd) queryResult
    return entries

stripResult :: Result a -> a
stripResult (Ok z) = z
stripResult (Error s) = error $ "JSON error: " ++ s

pasteToDB :: AcidState Blog -> Entry -> IO (EventResult InsertEntry)
pasteToDB acid !e = update' acid (InsertEntry e)

main :: IO()
main = do
    tbDir <- getEnv "TAZBLOG"
    bracket (openLocalStateFrom (tbDir ++ "/BlogState") initialBlogState)
            (createCheckpointAndClose)
            (\acid -> convertEntries acid)

convertEntries acid = do
    entries <- parseOldEntries
    let r =  map forceHack entries
    rs <- sequence r
    putStrLn $ show rs
  where
    forceHack !x = do
        xy <- pasteToDB acid x
        return $ show xy

testThis :: IO ()
testThis = do
  acid <- openLocalState initialBlogState
  allE <- query' acid LatestPosts
  putStrLn $ show allE