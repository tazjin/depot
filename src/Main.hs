{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving,
    DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses, TemplateHaskell, 
    TypeFamilies, RecordWildCards #-}

module Main where

import           Control.Applicative ((<$>), (<*>), optional, pure)
import           Control.Monad (msum)
import           Control.Monad.State (get, put)
import           Control.Monad.Reader (ask)
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Acid.Local
import           Data.ByteString.Char8 (ByteString)
import           Data.Data (Data, Typeable)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.SafeCopy (base, deriveSafeCopy)
import           Database.CouchDB
import           Happstack.Server
import           Network.CGI (liftIO)
import           Text.JSON.Generic
import           System.Locale (defaultTimeLocale)

import           Blog
import           Locales

{-session handling functions-}

data SessionState = SessionState { sessions :: [(String, Integer)] } -- id/date
  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''SessionState)

initialSession :: SessionState
initialSession = SessionState []

addSession :: (String, Integer) -> Update SessionState [(String, Integer)]
addSession newS = do
  s@SessionState{..} <- get
  let newSessions = newS : sessions
  put $ s{ sessions = newSessions }
  return newSessions

querySessions :: Query SessionState [(String, Integer)]
querySessions = sessions <$> ask

$(makeAcidic ''SessionState ['addSession, 'querySessions])

tmpPolicy :: BodyPolicy
tmpPolicy = (defaultBodyPolicy "./tmp/" 0 1000 1000)

main :: IO()
main = do
    putStrLn ("TazBlog " ++ version ++ " in Haskell starting")
    simpleHTTP nullConf tazBlog

tazBlog :: ServerPart Response
tazBlog = do
    msum [ dir (show DE) $ blogHandler DE
         , dir (show EN) $ blogHandler EN
         , do nullDir
              showIndex DE
         , do dir " " $ nullDir
              seeOther ("https://plus.google.com/115916629925754851590" :: String) (toResponse ())
         , path $ \(id_ :: Int) -> getEntryLink id_
         , path $ \(year :: Int) -> path $ \(month :: Int) -> path $ \(id_ :: String) -> formatOldLink year month id_
         , dir "res" $ serveDirectory DisableBrowsing [] "../res"
         , dir "notice" $ ok $ toResponse showSiteNotice
         , do adminSession <- lookCookieValue "session"
              ok $ toResponse ("Eingeloggt" :: String)
         , dir "admin" $ ok $ toResponse $ adminTemplate adminLogin "Login"
         , serveDirectory DisableBrowsing [] "../res"
         ]

blogHandler :: BlogLang -> ServerPart Response
blogHandler lang = 
    msum [ path $ \(year :: Int) -> path $ \(month :: Int) -> path $ --single entry
                      \(day :: Int) -> path $ \(id_ :: String) -> showEntry lang id_
         , path $ \(year :: Int ) -> path $ \(month :: Int) -> showMonth year month lang
         , do
            decodeBody tmpPolicy
            dir "postcomment" $ path $ \(id_ :: String) -> addComment id_
         , do nullDir
              showIndex lang
         ]

formatOldLink :: Int -> Int -> String -> ServerPart Response
formatOldLink y m id_ = 
  flip seeOther (toResponse ()) $ 
    concat $ intersperse' "/"  ["de", show y, show m, replace '.' '/' id_]

showEntry :: BlogLang -> String -> ServerPart Response
showEntry lang id_ = do
    entryJS <- liftIO $ runCouchDB' $ getDoc (db "tazblog") (doc id_)
    let entry = maybeDoc entryJS
    ok $ tryEntry entry lang

tryEntry :: Maybe Entry -> BlogLang -> Response
tryEntry Nothing lang = toResponse $ showError NotFound lang
tryEntry (Just entry) _ = toResponse $ blogTemplate eLang eTitle $ renderEntry entry
    where
        eTitle = T.pack $ ": " ++ title entry
        eLang = lang entry

getEntryLink :: Int -> ServerPart Response
getEntryLink id_ = do
    entryJS <- liftIO $ runCouchDB' $ getDoc (db "tazblog") (doc $ show id_)
    let entry = maybeDoc entryJS
    seeOther (makeLink entry) (toResponse())
  where
    makeLink :: Maybe Entry -> String
    makeLink (Just e) = concat $ intersperse' "/" [show $ lang e, show $ year e, show $ month e, show $ day e, show id_]
    makeLink Nothing = "/"

showIndex :: BlogLang -> ServerPart Response
showIndex lang = do
    entries <- getLatest lang [("descending", showJSON True)]
    (page :: Maybe Int) <- optional $ lookRead "page"
    ok $ toResponse $ blogTemplate lang "" $ 
        renderEntries False (eDrop page entries) (topText lang) (Just $ showLinks page lang)
  where
    eDrop :: Maybe Int -> [a] -> [a]
    eDrop (Just i) = drop ((i-1) * 6)
    eDrop Nothing = drop 0 
    
showMonth :: Int -> Int -> BlogLang -> ServerPart Response
showMonth y m lang = do
    entries <- getLatest lang $ ("descending", showJSON True) : makeQuery startkey endkey
    ok $ toResponse $ blogTemplate lang month 
        $ renderEntries True entries month Nothing
  where
    month = getMonth lang y  m
    startkey = JSArray [toJSON y, toJSON m]
    endkey = JSArray [toJSON y, toJSON m, JSObject (toJSObject [] )]

addComment :: String -> ServerPart Response
addComment id_ = do
  rda <- liftIO $ currentSeconds >>= return
  nComment <- Comment <$> look "cname"
                      <*> look "ctext"
                      <*> pure rda
  rev <- updateDBDoc (doc id_) $ insertComment nComment
  liftIO $ putStrLn $ show rev
  seeOther ("/" ++ id_) (toResponse())

-- http://tazj.in/2012/02/10.155234

currentSeconds :: IO Integer
currentSeconds = do
    now <- getCurrentTime
    let s =  read (formatTime defaultTimeLocale "%s" now) :: Integer
    return s

-- CouchDB functions
getLatest :: BlogLang -> [(String, JSValue)] -> ServerPart [Entry]
getLatest lang arg = do
        queryResult <- queryDB view arg
        let entries = map (stripResult . fromJSON . snd) queryResult
        return entries
    where
        view = case lang of
                EN -> "latestEN"
                DE -> "latestDE"

insertComment :: Comment -> JSValue -> IO JSValue
insertComment c jEntry = return $ toJSON $ e { comments = c : (comments e)}
  where
    e = convertJSON jEntry :: Entry

makeQuery :: JSON a => a -> a -> [(String, JSValue)]
makeQuery qsk qek = [("startkey", (showJSON qsk))
                    ,("endkey", (showJSON qek))]

queryDB :: JSON a => String -> [(String, JSValue)] -> ServerPart [(Doc, a)]
queryDB view arg = liftIO . runCouchDB' $ queryView (db "tazblog") (doc "entries") (doc view) arg

maybeDoc :: Data a => Maybe (Doc, Rev, JSValue) -> Maybe a
maybeDoc (Just(_,_,v)) = Just( stripResult $ fromJSON v)
maybeDoc Nothing = Nothing

updateDBDoc :: JSON a => Doc -> (a -> IO a) -> ServerPart (Maybe Rev)
updateDBDoc docn f = liftIO $ runCouchDB' $ getAndUpdateDoc (db "tazblog") docn f

stripResult :: Result a -> a
stripResult (Ok z) = z
stripResult (Error s) = error $ "JSON error: " ++ s

convertJSON :: Data a => JSValue -> a
convertJSON = stripResult . fromJSON

getMonthCount :: BlogLang -> Int -> Int -> ServerPart Int
getMonthCount lang y m  = do
    count <- queryDB (view lang) $ makeQuery startkey endkey
    return . stripCount $ map (stripResult . fromJSON . snd) count
  where
    startkey = JSArray [toJSON ("count" :: String), toJSON y, toJSON m]
    endkey = JSArray [toJSON ("count" :: String), toJSON y, toJSON m, JSObject (toJSObject [] )]
    stripCount :: [Int] -> Int
    stripCount [x] = x
    stripCount [] = 0
    view DE = "countDE"
    view EN = "countEN"


-- CouchDB View Setup
latestDEView = "function(doc){ if(doc.lang == 'DE'){ emit([doc.year, doc.month, doc.day, doc._id], doc); } }"
latestENView = "function(doc){ if(doc.lang == 'EN'){ emit([doc.year, doc.month, doc.day, doc._id], doc); } }"
countDEView  = "function(doc){ if(doc.lang == 'DE'){ emit(['count', doc.year, doc.month, doc.day, doc._id], 1); } }"
countENView  = "function(doc){ if(doc.lang == 'EN'){ emit(['count', doc.year, doc.month, doc.day, doc._id], 1); } }"
countReduce  = "function(keys, values, rereduce) { return sum(values); }"

latestDE = ViewMap "latestDE" latestDEView
latestEN = ViewMap "latestEN" latestENView
countDE  = ViewMapReduce "countDE" countDEView countReduce
countEN  = ViewMapReduce "countEN" countENView countReduce

setupBlogViews :: IO ()
setupBlogViews = runCouchDB' $ 
    newView "tazblog" "entries" [latestDE, latestEN, countDE, countEN]
