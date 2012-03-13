{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving,
    DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses, TemplateHaskell, 
    TypeFamilies, RecordWildCards, BangPatterns #-}

module Main where

import           Control.Applicative ((<$>), (<*>), optional, pure)
import           Control.Exception (bracket)
import           Control.Monad (msum, mzero, when, unless)
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
import           Network.CGI (liftIO)
import           System.Environment(getEnv)
import           System.Locale (defaultTimeLocale)

import           Blog
import           BlogDB hiding (addComment)
import           Locales

{- Server -}

tmpPolicy :: BodyPolicy
tmpPolicy = (defaultBodyPolicy "./tmp/" 0 1000 1000)

main :: IO()
main = do
    putStrLn ("TazBlog " ++ version ++ " in Haskell starting")
    tbDir <- getEnv "TAZBLOG"
    bracket (openLocalStateFrom (tbDir ++ "/BlogState") initialBlogState)
            (createCheckpointAndClose)
            (\acid -> simpleHTTP nullConf $ tazBlog acid)

tazBlog :: AcidState Blog -> ServerPart Response
tazBlog acid = do
    msum [ dir (show DE) $ blogHandler acid DE
         , dir (show EN) $ blogHandler acid EN
         , do nullDir
              showIndex acid DE
         , do dir " " $ nullDir
              seeOther ("https://plus.google.com/115916629925754851590" :: Text) (toResponse ())
         , path $ \(year :: Int) -> path $ \(month :: Int) -> path $ \(id_ :: String) -> formatOldLink year month id_
         , dir "res" $ serveDirectory DisableBrowsing [] "../res"
         , dir "notice" $ ok $ toResponse showSiteNotice
         , do dir "admin" $ guardSession acid
              adminHandler
         , dir "admin" $ ok $ toResponse $ adminTemplate adminLogin "Login"
         , dir "dologin" $ processLogin acid
         , serveDirectory DisableBrowsing [] "../res"
         ]

blogHandler :: AcidState Blog -> BlogLang -> ServerPart Response
blogHandler acid lang = 
    msum [ path $ \(eId :: Integer) -> showEntry acid lang $ EntryId eId
         , do
            decodeBody tmpPolicy
            dir "postcomment" $ path $ 
              \(eId :: Integer) -> addComment acid $ EntryId eId
         , do nullDir
              showIndex acid lang
         ]

guardSession :: AcidState Blog -> ServerPartT IO ()
guardSession acid = do
    (sId :: Text) <- readCookieValue "session"
    (Just Session{..}) <- query' acid (GetSession $ SessionID sId)
    (uName :: Text) <- readCookieValue "sUser"
    now <- liftIO $ getCurrentTime
    unless (and [uName == username user, sessionTimeDiff now sdate])
      mzero
  where
    sessionTimeDiff :: UTCTime -> UTCTime -> Bool
    sessionTimeDiff now sdate = (diffUTCTime now sdate) > 43200

adminHandler :: ServerPart Response
adminHandler = undefined

formatOldLink :: Int -> Int -> String -> ServerPart Response
formatOldLink y m id_ = 
  flip seeOther (toResponse ()) $ 
    concat $ intersperse' "/"  ["de", show y, show m, replace '.' '/' id_]

showEntry :: AcidState Blog -> BlogLang -> EntryId -> ServerPart Response
showEntry acid lang eId = do
    entry <- query' acid (GetEntry eId)
    ok $ tryEntry entry lang

tryEntry :: Maybe Entry -> BlogLang -> Response
tryEntry Nothing lang = toResponse $ showError NotFound lang
tryEntry (Just entry) _ = toResponse $ blogTemplate eLang eTitle $ renderEntry entry
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

addComment :: AcidState Blog -> EntryId -> ServerPart Response
addComment acid eId = do
  now <- liftIO $ getCurrentTime >>= return
  nComment <- Comment <$> lookText' "cname"
                      <*> lookText' "ctext"
                      <*> pure now
  update' acid (AddComment eId nComment)
  seeOther ("/" ++ show eId) (toResponse())

processLogin :: AcidState Blog -> ServerPart Response
processLogin acid = do
    decodeBody tmpPolicy
    account <- lookText' "account"
    password <- look "password"
    login <- query' acid (CheckUser (Username account) password)
    if' login
      (createSession account)
      (ok $ toResponse $ adminTemplate adminLogin "Login failed")
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

