{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving,
    DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses, TemplateHaskell, 
    TypeFamilies, RecordWildCards, BangPatterns #-}

module Main where

import           Control.Applicative ((<$>), (<*>), optional, pure)
import           Control.Exception (bracket)
import           Control.Monad (msum, mzero, when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (get, put)
import           Control.Monad.Reader (ask)
import qualified Crypto.Hash.SHA512 as SHA
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Acid.Local
import qualified Data.ByteString.Base64 as B64 (encode)
import           Data.ByteString.Char8 (ByteString, pack, unpack)
import           Data.Data (Data, Typeable)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.SafeCopy (base, deriveSafeCopy)
import           Happstack.Server hiding (Session)
import           Happstack.Server.Compression
import           System.Environment(getEnv)
import           System.Locale (defaultTimeLocale)

import           Blog
import           BlogDB hiding (addComment, updateEntry)
import           Locales

{- Server -}

tmpPolicy :: BodyPolicy
tmpPolicy = (defaultBodyPolicy "./tmp/" 0 200000 1000)

main :: IO()
main = do
    putStrLn ("TazBlog " ++ version ++ " in Haskell starting")
    tbDir <- getEnv "TAZBLOG"
    bracket (openLocalStateFrom (tbDir ++ "/BlogState") initialBlogState)
            (createCheckpointAndClose)
            (\acid -> simpleHTTP nullConf {port = 80} $ tazBlog acid)

tazBlog :: AcidState Blog -> ServerPart Response
tazBlog acid = do
    compr <- compressedResponseFilter
    msum [ dir (show DE) $ blogHandler acid DE
         , dir (show EN) $ blogHandler acid EN
         , do nullDir
              showIndex acid DE
         , do dir " " $ nullDir
              seeOther ("https://plus.google.com/115916629925754851590" :: Text) (toResponse ())
         , path $ \(year :: Int) -> path $ \(month :: Int) -> path $ \(id_ :: String) -> formatOldLink year month id_
         , dir "res" $ serveDirectory DisableBrowsing [] "../res"
         , dir "notice" $ ok $ toResponse showSiteNotice
         {- :Admin handlers -}
         , do dirs "admin/postentry" $ nullDir
              guardSession acid
              postEntry acid
         , do dirs "admin/entrylist" $ dir (show DE) $ nullDir
              guardSession acid
              entryList acid DE
         , do dirs "admin/entrylist" $ dir (show EN) $ nullDir
              guardSession acid
              entryList acid EN
         , do guardSession acid
              dirs "admin/edit" $ path $ \(eId :: Integer) -> editEntry acid eId
         , do dirs "admin/updateentry" $ nullDir
              updateEntry acid
         , do dir "admin" $ nullDir
              guardSession acid
              ok $ toResponse $ adminIndex ("tazjin" :: Text)
         , dir "admin" $ ok $ toResponse $ adminLogin 
         , dir "dologin" $ processLogin acid
         , do setHeaderM "cache-control" "max-age=630720000"
              setHeaderM "expires" "Tue, 20 Jan 2037 04:20:42 GMT"
              dir "static" $ serveDirectory DisableBrowsing [] "../res"
         , serveDirectory DisableBrowsing [] "../res"
         , notFound $ toResponse $ showError NotFound DE
         ]

blogHandler :: AcidState Blog -> BlogLang -> ServerPart Response
blogHandler acid lang = 
    msum [ path $ \(eId :: Integer) -> showEntry acid lang $ EntryId eId
         , do decodeBody tmpPolicy
              dir "postcomment" $ path $ 
                \(eId :: Integer) -> addComment acid lang $ EntryId eId
         , do nullDir
              showIndex acid lang
         , notFound $ toResponse $ showError NotFound lang
         ]

formatOldLink :: Int -> Int -> String -> ServerPart Response
formatOldLink y m id_ = 
  flip seeOther (toResponse ()) $ 
    concat $ intersperse' "/"  ["de", show y, show m, replace '.' '/' id_]

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
        renderEntries False (eDrop page entries) (topText lang) (Just $ showLinks page lang)
  where
    eDrop :: Maybe Int -> [a] -> [a]
    eDrop (Just i) = drop ((i-1) * 6)
    eDrop Nothing = drop 0 

addComment :: AcidState Blog -> BlogLang -> EntryId -> ServerPart Response
addComment acid lang eId = do
  now <- liftIO $ getCurrentTime >>= return
  nCtext <- lookText' "ctext"
  nComment <- Comment <$> pure now
                      <*> lookText' "cname"
                      <*> pure (commentEscape nCtext)
  update' acid (AddComment eId nComment)
  seeOther ("/" ++ show lang ++ "/" ++ show eId) (toResponse())

commentEscape :: Text -> Text
commentEscape = newlineEscape . ltEscape . gtEscape . ampEscape
    where
        newlineEscape = T.replace "\n" "<br>"
        ampEscape = T.replace "&" "&amp;"
        ltEscape = T.replace "<" "&lt;"
        gtEscape = T.replace ">" "&gt;"

{- ADMIN stuff -} 

postEntry :: AcidState Blog -> ServerPart Response
postEntry acid = do
    decodeBody tmpPolicy
    now <- liftIO $ getCurrentTime
    let eId = timeToId now
    lang <- look "lang"
    nBtext <- lookText' "btext"
    nMtext <- lookText' "mtext"
    nEntry <- Entry <$> pure eId
                    <*> getLang lang
                    <*> readCookieValue "sUser"
                    <*> lookText' "title"
                    <*> pure (entryEscape nBtext)
                    <*> pure (entryEscape nMtext)
                    <*> pure now
                    <*> pure [] -- NYI
                    <*> pure []
    update' acid (InsertEntry nEntry)
    seeOther ("/" ++ lang ++ "/" ++ show eId) (toResponse())
  where
    timeToId :: UTCTime -> EntryId
    timeToId t = EntryId . read $ formatTime defaultTimeLocale "%s" t
    getLang :: String -> ServerPart BlogLang
    getLang "de" = return DE
    getLang "en" = return EN

entryEscape :: Text -> Text
entryEscape = newlineEscape . newlineRNEscape
  where
    newlineEscape = T.replace "\n" "<br>"
    newlineRNEscape = T.replace "\r\n" "<br>"

entryList :: AcidState Blog -> BlogLang -> ServerPart Response
entryList acid lang = do
    entries <- query' acid (LatestEntries lang)
    ok $ toResponse $ adminEntryList entries

editEntry :: AcidState Blog -> Integer -> ServerPart Response
editEntry acid i = do
    (Just entry) <- query' acid (GetEntry eId)
    ok $ toResponse $ editPage entry
  where
    eId = EntryId i

updateEntry :: AcidState Blog -> ServerPart Response
updateEntry acid = do
    decodeBody tmpPolicy
    (eId :: Integer) <- lookRead "eid"
    (Just entry) <- query' acid (GetEntry $ EntryId eId)
    nTitle <- lookText' "title"
    nBtext <- lookText' "btext"
    nMtext <- lookText' "mtext"
    let nEntry = entry { title = nTitle
                        , btext = entryEscape nBtext
                        , mtext = entryEscape nMtext}
    update' acid (UpdateEntry nEntry)
    seeOther (concat $ intersperse' "/" [show $ lang entry, show eId])
             (toResponse ())


guardSession :: AcidState Blog -> ServerPartT IO ()
guardSession acid = do
    (sId :: Text) <- readCookieValue "session"
    (uName :: Text) <- readCookieValue "sUser"
    now <- liftIO $ getCurrentTime
    mS <- query' acid (GetSession $ SessionID sId)
    case mS of
      Nothing -> mzero
      (Just Session{..}) -> unless (and [ uName == username user
                                        , sessionTimeDiff now sdate])
                                    mzero
  where
    sessionTimeDiff :: UTCTime -> UTCTime -> Bool
    sessionTimeDiff now sdate = (diffUTCTime now sdate) < 43200


processLogin :: AcidState Blog -> ServerPart Response
processLogin acid = do
    decodeBody tmpPolicy
    account <- lookText' "account"
    password <- look "password"
    login <- query' acid (CheckUser (Username account) password)
    if' login
      (createSession account)
      (ok $ toResponse $ adminLogin)
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

