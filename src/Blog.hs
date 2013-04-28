{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Blog where

import           BlogDB
import           Control.Monad             (unless, when)
import           Data.Data                 (Data, Typeable)
import           Data.List                 (intersperse)
import           Data.Maybe                (fromJust)
import           Data.Monoid               (mempty)
import           Data.Text                 (Text, append, empty, pack)
import           Data.Text.Lazy            (fromStrict)
import           Data.Time
import           Locales
import           Network.Captcha.ReCaptcha
import           System.Locale             (defaultTimeLocale)
import           Text.Blaze.Html           (preEscapedToHtml)
import           Text.Hamlet
import           Text.Lucius
import           Text.Markdown

import qualified Data.Text                 as T

-- custom list functions
intersperse' :: a -> [a] -> [a]
intersperse' sep l = sep : intersperse sep l

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

show' :: Show a => a -> Text
show' = pack . show

-- |After this time all entries are Markdown
markdownCutoff :: UTCTime
markdownCutoff = fromJust $ parseTime defaultTimeLocale "%s" "1367149834"


-- blog CSS (admin is still static)
stylesheetSource = $(luciusFile "res/blogstyle.lucius")
blogStyle = renderCssUrl undefined stylesheetSource

-- blog HTML
blogTemplate :: BlogLang -> Text -> Html -> Html
blogTemplate lang t_append body = [shamlet|
$doctype 5
 <head>
  <title>#{blogTitle lang t_append}
  <link rel="stylesheet" type="text/css" href="/static/blogv34.css" media="all">
  <link rel="alternate" type="application/rss+xml" title="RSS-Feed" href=#{rssUrl}>
  <meta http-equiv="content-type" content="text/html;charset=UTF-8">
 <body>
  <div class="header">
   <a class="btitle" href=#{append "/" (show' lang)}>#{blogTitle lang empty}
   <p style="clear: both;">
    <span class="contacts" id="cosx">^{contactInfo}
    <span class="righttext">^{preEscapedToHtml $ rightText lang}
  <div class="middle">
   ^{body}
   <div class="footer">
    ^{showFooter lang $ pack version}
    <div class="centerbox">
     <span style="font-size:17px;font-family:Helvetica;">ಠ_ಠ
|]
 where
  rssUrl = T.concat ["/", show' lang, "/rss.xml"]
  contactInfo = [shamlet|
#{contactText lang}
<a class="link" href=#{mailTo}>Mail
#{orText lang}
<a class="link" href=#{twitter} target="_blank">Twitter
|]

showFooter :: BlogLang -> Text -> Html
showFooter l v = [shamlet|
<div class="rightbox" style="text-align:right;">
 Proudly made with #
 <a class="link" href="http://haskell.org">Haskell
 , #
 <a class="link" href="http://hackage.haskell.org/package/acid-state-0.6.3">Acid-State
 \ and without PHP, Java, Perl, MySQL and Python.
 <p>
  <a class="link" href=#{repoURL}>#{append "Version " v}
  &nbsp;
  <a class="link" href="/notice">#{noticeText l}
|]

isEntryMarkdown :: Entry -> Bool
isEntryMarkdown e = edate e > markdownCutoff

renderEntryMarkdown :: Text -> Html
renderEntryMarkdown = markdown def {msXssProtect = False} . fromStrict

renderEntries :: Bool -> [Entry] -> Text -> Maybe Html -> Html
renderEntries showAll entries topText footerLinks = [shamlet|
<span class="innerTitle">#{topText}
<div class="innerContainer">
 <ul style="max-width:57em;">
  $forall entry <- elist
   <li>
    $if (isEntryMarkdown entry)
      <a href=#{linkElems entry}>#{linkText $ length $ comments entry}
      <b>#{title entry}
      ^{renderEntryMarkdown $ append " " $ btext entry}
    $else
      <a href=#{linkElems entry}>#{linkText $ length $ comments entry}
      ^{preEscapedToHtml $ append " " $ btext entry}
    $if ((/=) (mtext entry) empty)
     <p><a href=#{linkElems entry}>#{readMore $ lang entry}
    $else
     <br>&nbsp;
 $maybe links <- footerLinks
  ^{links}
|]
  where
   elist = if' showAll entries (take 6 entries)
   linkElems Entry{..} = concat $ intersperse' "/" [show lang, show entryId]
   linkText n = T.concat ["[", show' n, "]"]

showLinks :: Maybe Int -> BlogLang -> Html
showLinks (Just i) lang = [shamlet|
 $if ((>) i 1)
  <div class="centerbox">
   <a href=#{nLink $ succ i}>#{backText lang}
   \ -- #
   <a href=#{nLink $ pred i}>#{nextText lang}
 $elseif ((<=) i 1)
  ^{showLinks Nothing lang}
|]
  where
   nLink page = T.concat ["/", show' lang, "/?page=", show' page]
showLinks Nothing lang = [shamlet|
<div class="centerbox">
 <a href=#{nLink}>#{backText lang}
|]
  where
   nLink = T.concat ["/", show' lang, "/?page=2"]

renderEntry :: Entry -> Html
renderEntry e@Entry{..} = [shamlet|
<span class="innerTitle">#{title}
<span class="righttext">
 <i>#{woText}
<div class="innerContainer">
 <article>
  <ul style="max-width:57em;">
   <li>
    $if (isEntryMarkdown e)
      ^{renderEntryMarkdown btext}
      <p>^{renderEntryMarkdown $ mtext}
    $else
      ^{preEscapedToHtml $ btext}
      <p>^{preEscapedToHtml $ mtext}
 <div class="innerBoxComments">
  <div class="cHead">#{cHead lang}
  <ul style="max-width:57em;">#{renderComments comments lang}
  ^{renderCommentBox lang entryId}
|]
  where
   woText = flip T.append author $ T.pack $ formatTime defaultTimeLocale (eTimeFormat lang) edate

renderComments :: [Comment] -> BlogLang -> Html
renderComments [] lang = [shamlet|<li>#{noComments lang}|]
renderComments comments lang = [shamlet|
$forall comment <- comments
 <li>
  <i>#{append (cauthor comment) ": "}
  ^{preEscapedToHtml $ ctext comment}
  <p class="tt">#{timeString $ cdate comment}
|]
  where
   timeString = formatTime defaultTimeLocale (cTimeFormat lang)

captcha :: Html
captcha = [shamlet|
<div class="cCaptcha">
  <script src="http://api.recaptcha.net/challenge?k=6LfQXccSAAAAAIjKm26XlFnBMAgvaKlOAjVWEEnM" type="text/javascript">
  <noscript>
    <iframe src="http://api.recaptcha.net/noscript?k=6LfQXccSAAAAAIjKm26XlFnBMAgvaKlOAjVWEEnM" height="300" width="500" seamless>
      <br>
      <textarea name="recaptcha_challenge_field" rows="3" cols="40">
      <input type="hidden" name="recaptcha_response_field" value="manual_challenge">
|]

captchaOptions :: BlogLang ->  Html
captchaOptions lang = [shamlet|<script type="text/javascript">^{preEscapedToHtml options}|]
  where
    options = T.concat ["var RecaptchaOptions = { theme: 'clean', lang: '", showLangText lang, "'};"]


renderCommentBox :: BlogLang -> EntryId -> Html
renderCommentBox cLang cId = [shamlet|
<div class="cHead">#{cwHead cLang}
^{captchaOptions cLang}
<form method="POST" action=#{aLink}>
 <p><input name="cname" placeholder="Name" class="cInput">
 <p>
  <label>
   <textarea name="ctext" cols="50" rows="13" class="cInput" placeholder=#{cTextPlaceholder cLang}>
 <p>
  <label>
   ^{captcha}
 <p><input class="cInput" style="width:120px;" type="submit" value=#{cSend cLang}>
|]
  where
   aLink = T.concat ["/", show' cLang, "/postcomment/", show' cId]

showSiteNotice :: Html
showSiteNotice = [shamlet|
$doctype 5
<head>
 <title>Impressum
<body>
 <h2>Impressum
 <br>
 <p>
  Vincent Ambo
  <br>
  Gyllenborgsgatan 8, LGH 1306
  <br>
  11243 Stockholm
  <p><a href="/" style="color:black;">Back
|]

{- Administration pages -}

adminTemplate :: Text -> Html -> Html
adminTemplate title body = [shamlet|
$doctype 5
<head>
 <link rel="stylesheet" type="text/css" href="/static/admin.css" media="all">
 <meta http-equiv="content-type" content="text/html;charset=UTF-8">
 <title>#{append "TazBlog Admin: " title}
<body>
 ^{body}
|]

adminLogin :: Html
adminLogin = adminTemplate "Login" $ [shamlet|
<div class="loginBox">
 <div class="loginBoxTop">TazBlog Admin: Login
 <div class="loginBoxMiddle">
  <form action="/dologin" method="POST">
   <p>Account ID
   <p><input type="text" style="font-size:2;" name="account" value="tazjin" readonly="1">
   <p>Passwort
   <p><input type="password" style="font-size:2;" name="password">
   <p><input alt="Anmelden" type="image" src="/static/signin.gif">
|]

adminIndex :: Text -> Html
adminIndex sUser = adminTemplate "Index" $ [shamlet|
<div style="float:center;">
 <form action="/admin/postentry" method="POST">
  <table>
   <tr>
    <thead><td>Titel:
    <td><input type="text" name="title">
   <tr>
    <thead><td>Sprache:
    <td><select name="lang">
     <option value="de">Deutsch
     <option value="en">Englisch
   <tr>
    <thead><td>Text:
    <td>
     <textarea name="btext" cols="100" rows="15">
   <tr>
    <thead>
     <td style="vertical-align:top;">Mehr Text:
    <td>
     <textarea name="mtext" cols="100" rows="15">
  <input type="hidden" name="author" value=#{sUser}>
  <input style="margin-left:20px;" type="submit" value="Absenden">
 ^{adminFooter}
|]

adminFooter :: Html
adminFooter = [shamlet|
<a href="/">Startseite
\ -- Entrylist: #
<a href="/admin/entrylist/de">DE
\ & #
<a href="/admin/entrylist/en">EN
\ -- #
<a href="#">Backup
\ (NYI)
|]

adminEntryList :: [Entry] -> Html
adminEntryList entries = adminTemplate "EntryList" $ [shamlet|
<div style="float: center;">
 <table>
  $forall entry <- entries
   <tr>
    <td><a href=#{append "/admin/edit/" (show' $ entryId entry)}>#{title entry}
    <td>#{formatPostDate $ edate entry}
|]
 where
  formatPostDate = formatTime defaultTimeLocale "[On %D at %H:%M]"

editPage :: Entry -> Html
editPage (Entry{..}) = adminTemplate "Index" $ [shamlet|
<div style="float:center;">
 <form action="/admin/updateentry" method="POST">
  <table>
   <tr>
    <td>Titel:
    <td>
     <input type="text" name="title" value=#{title}>
   <tr>
    <td style="vertical-align:top;">Text:
    <td>
     <textarea name="btext" cols="100" rows="15">#{btext}
   <tr>
    <td style="vertical-align:top;">Mehr Text:
    <td>
     <textarea name="mtext" cols="100" rows="15">#{mtext}
  <input type="hidden" name="eid" value=#{unEntryId entryId}>
  <input type="submit" style="margin-left:20px;" value="Absenden">
  <div class="editComments">#{editComments comments entryId}
  <p>^{adminFooter}
|]

editComments :: [Comment] -> EntryId -> Html
editComments comments eId = [shamlet|
<table>
 $forall c <- comments
  <tr>
   <td>#{cauthor c}
   <td>#{cPostTime $ cdate c}
  <tr>
   <td><a href=#{cDeleteLink $ cdate c}>Löschen
|]
 where
  cPostTime = formatTime defaultTimeLocale "%c"
  cDeleteLink cd = concat ["/admin/cdelete/", show eId, formatTime defaultTimeLocale "/%s%Q" cd]

commentDeleted :: EntryId -> Html
commentDeleted eId = adminTemplate "Kommentar gelöscht" $ [shamlet|
<div>Der Kommentar wurde gelöscht.
<br>
<a href=#{append "/de/" $ show' eId}>Eintrag ansehen | #
<a href=#{append "/admin/edit/" $ show' eId}>Eintrag bearbeiten
|]

showError :: BlogError -> BlogLang -> Html
showError NotFound l = blogTemplate l (T.append ": " $ notFoundTitle l) $ [shamlet|
<span class="innerTitle">#{notFoundTitle l}
<div class="innerTitle">
 <p class="notFoundFace">:(
 <p class="notFoundText">#{notFoundText l}
|]

