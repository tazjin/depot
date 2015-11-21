-- Server implementation based on Happstack

module Server where

import           Control.Applicative    (optional)
import           Control.Monad          (msum, mzero, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Acid
import           Data.Acid.Advanced
import           Data.ByteString.Char8  (unpack)
import           Data.Char              (toLower)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time
import           Happstack.Server       hiding (Session)

import Blog
import BlogDB  hiding (updateEntry)
import Locales
import RSS

instance FromReqURI BlogLang where
  fromReqURI sub =
    case map toLower sub of
      "de" -> Just DE
      "en" -> Just EN
      _    -> Nothing

tmpPolicy :: BodyPolicy
tmpPolicy = defaultBodyPolicy "/tmp" 0 200000 1000

runBlog :: AcidState Blog -> Int -> String -> IO ()
runBlog acid port respath =
  simpleHTTP nullConf {port = port} $ tazBlog acid respath

tazBlog :: AcidState Blog -> String -> ServerPart Response
tazBlog acid resDir = do
    msum [ path $ \(lang :: BlogLang) -> blogHandler acid lang
         , dir "admin" $ msum [
                adminHandler acid -- this checks auth
              , method GET >> (ok $ toResponse adminLogin)
              , method POST >> processLogin acid ]
         , dir "static" $ staticHandler resDir
         , blogHandler acid EN
         , notFound $ toResponse $ showError NotFound DE
         ]

blogHandler :: AcidState Blog -> BlogLang -> ServerPart Response
blogHandler acid lang =
    msum [ path $ \(eId :: Integer) -> showEntry acid lang $ EntryId eId
         , nullDir >> showIndex acid lang
         , dir "rss" $ nullDir >> showRSS acid lang
         , dir "rss.xml" $ nullDir >> showRSS acid lang
         , notFound $ toResponse $ showError NotFound lang
         ]

staticHandler :: String -> ServerPart Response
staticHandler resDir = do
  setHeaderM "cache-control" "max-age=630720000"
  setHeaderM "expires" "Tue, 20 Jan 2037 04:20:42 GMT"
  serveDirectory DisableBrowsing [] resDir

serveBlogStyle :: ServerPart Response
serveBlogStyle = do
  setHeaderM "content-type" "text/css"
  setHeaderM "cache-control" "max-age=630720000"
  setHeaderM "expires" "Tue, 20 Jan 2037 04:20:42 GMT"
  ok $ toResponse $ blogStyle

adminHandler :: AcidState Blog -> ServerPart Response
adminHandler acid = do
  guardSession acid
  msum [ dir "entry" $ method POST >> postEntry acid
       , dir "entry" $ path $ \(entry :: Integer) -> msum [
              method GET >> editEntry acid entry
            , method POST >> updateEntry acid entry ]
       , dir "entrylist" $ path $ \(lang :: BlogLang) -> entryList acid lang
       , ok $ toResponse $ adminIndex "tazjin"
       ]

showEntry :: AcidState Blog -> BlogLang -> EntryId -> ServerPart Response
showEntry acid lang eId = do
    entry <- query' acid (GetEntry eId)
    tryEntry entry lang

tryEntry :: Maybe Entry -> BlogLang -> ServerPart Response
tryEntry Nothing lang = notFound $ toResponse $ showError NotFound lang
tryEntry (Just entry) _ = ok $ toResponse $ blogTemplate eLang eTitle $ renderEntry entry
    where
        eTitle = T.append ": " (title entry)
        eLang = lang entry

showIndex :: AcidState Blog -> BlogLang -> ServerPart Response
showIndex acid lang = do
    entries <- query' acid (LatestEntries lang)
    (page :: Maybe Int) <- optional $ lookRead "page"
    ok $ toResponse $ blogTemplate lang "" $
        renderEntries False (eDrop page entries) (Just $ showLinks page lang)
  where
    eDrop :: Maybe Int -> [a] -> [a]
    eDrop (Just i) = drop ((i-1) * 6)
    eDrop Nothing = drop 0

showRSS :: AcidState Blog -> BlogLang -> ServerPart Response
showRSS acid lang = do
    entries <- query' acid (LatestEntries lang)
    feed <- liftIO $ renderFeed lang $ take 6 entries
    setHeaderM "content-type" "text/xml"
    ok $ toResponse feed

{- ADMIN stuff -}

postEntry :: AcidState Blog -> ServerPart Response
postEntry acid = do
    nullDir
    decodeBody tmpPolicy
    now <- liftIO getCurrentTime
    let eId = timeToId now
    lang <- look "lang"
    nBtext <- lookText' "btext"
    nMtext <- lookText' "mtext"
    nEntry <- Entry <$> pure eId
                    <*> getLang lang
                    <*> readCookieValue "sUser"
                    <*> lookText' "title"
                    <*> pure nBtext
                    <*> pure nMtext
                    <*> pure now
    update' acid (InsertEntry nEntry)
    seeOther ("/" ++ lang ++ "/" ++ show eId) (toResponse())
  where
    timeToId :: UTCTime -> EntryId
    timeToId t = EntryId . read $ formatTime defaultTimeLocale "%s" t
    getLang :: String -> ServerPart BlogLang
    getLang "de" = return DE
    getLang _ = return EN -- English is default

entryList :: AcidState Blog -> BlogLang -> ServerPart Response
entryList acid lang = do
    entries <- query' acid (LatestEntries lang)
    ok $ toResponse $ adminEntryList entries

editEntry :: AcidState Blog -> Integer -> ServerPart Response
editEntry acid entryId = do
    (Just entry) <- query' acid (GetEntry $ EntryId entryId)
    ok $ toResponse $ editPage entry

updateEntry :: AcidState Blog -> Integer -> ServerPart Response
updateEntry acid entryId = do
    decodeBody tmpPolicy
    (Just entry) <- query' acid (GetEntry $ EntryId entryId)
    nTitle <- lookText' "title"
    nBtext <- lookText' "btext"
    nMtext <- lookText' "mtext"
    let newEntry = entry { title = nTitle
                         , btext = nBtext
                         , mtext = nMtext}
    update' acid (UpdateEntry newEntry)
    seeOther (concat $ intersperse' "/" [show $ lang entry, show entryId])
             (toResponse ())

guardSession :: AcidState Blog -> ServerPartT IO ()
guardSession acid = do
    (sId :: Text) <- readCookieValue "session"
    (uName :: Text) <- readCookieValue "sUser"
    now <- liftIO getCurrentTime
    mS <- query' acid (GetSession $ SessionID sId)
    case mS of
      Nothing -> mzero
      (Just Session{..}) -> unless ((uName == username user) && sessionTimeDiff now sdate)
                                   mzero
  where
    sessionTimeDiff :: UTCTime -> UTCTime -> Bool
    sessionTimeDiff now sdate = diffUTCTime now sdate < 43200


processLogin :: AcidState Blog -> ServerPart Response
processLogin acid = do
    decodeBody tmpPolicy
    account <- lookText' "account"
    password <- look "password"
    login <- query' acid (CheckUser (Username account) password)
    if login
      then createSession account
      else unauthorized $ toResponse adminLogin
  where
    createSession account = do
      now <- liftIO getCurrentTime
      let sId = hashString $ show now
      addCookie (MaxAge 43200) (mkCookie "session" $ unpack sId)
      addCookie (MaxAge 43200) (mkCookie "sUser" $ T.unpack account)
      (Just user) <- query' acid (GetUser $ Username account)
      let nSession = Session (T.pack $ unpack sId) user now
      update' acid (AddSession nSession)
      seeOther ("/admin?do=login" :: Text) (toResponse())
