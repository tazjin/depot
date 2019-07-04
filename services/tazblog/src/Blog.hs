{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Blog where

import           BlogDB
import           Data.Maybe      (fromJust)
import           Data.Text       (Text, append, empty, pack)
import           Data.Text.Lazy  (fromStrict)
import           Data.Time
import           Locales
import           Text.Blaze.Html (preEscapedToHtml)
import           Text.Hamlet
import           Text.Markdown

import qualified Data.Text       as T

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

show' :: Show a => a -> Text
show' = pack . show

-- |After this time all entries are Markdown
markdownCutoff :: UTCTime
markdownCutoff = fromJust $ parseTimeM False defaultTimeLocale "%s" "1367149834"

-- blog HTML
blogTemplate :: BlogLang -> Text -> Html -> Html
blogTemplate lang t_append body = [shamlet|
$doctype 5
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content=#{blogTitle lang t_append}>
    <link rel="stylesheet" type="text/css" href="/static/blog.css" media="all">
    <link rel="alternate" type="application/rss+xml" title="RSS-Feed" href=#{rssUrl}>
    <title>#{blogTitle lang t_append}
  <body>
    <header>
      <h1>
        <a href="/" .unstyled-link>#{blogTitle lang empty}
      <hr>
    ^{body}
    ^{showFooter}
|]
 where
  rssUrl = T.concat ["/", show' lang, "/rss.xml"]

showFooter :: Html
showFooter = [shamlet|
<footer>
  <p .footer>Served without any dynamic languages.
  <p .footer>
    <a href=#{repoURL} .uncoloured-link>Version #{version}
    |
    <a href=#{twitter} .uncoloured-link>Twitter
    |
    <a href=#{mailTo} .uncoloured-link>Mail
  <p .lod>
    ಠ_ಠ
|]

isEntryMarkdown :: Entry -> Bool
isEntryMarkdown e = edate e > markdownCutoff

renderEntryMarkdown :: Text -> Html
renderEntryMarkdown = markdown def {msXssProtect = False} . fromStrict

renderEntries :: Bool -> [Entry] -> Maybe Html -> Html
renderEntries showAll entries pageLinks = [shamlet|
$forall entry <- toDisplay
  <article>
    <h2 .inline>
      <a href=#{linkElems entry} .unstyled-link>
        #{title entry}
    <aside .date>
      #{pack $ formatTime defaultTimeLocale "%Y-%m-%d" $ edate entry}
    $if (isEntryMarkdown entry)
      ^{renderEntryMarkdown $ btext entry}
    $else
      ^{preEscapedToHtml $ btext entry}
    $if ((/=) (mtext entry) empty)
      <p>
        <a .uncoloured-link href=#{linkElems entry}>
          #{readMore $ lang entry}
  <hr>
$maybe links <- pageLinks
  ^{links}
|]
  where
   toDisplay = if showAll then entries else (take 6 entries)
   linkElems Entry{..} = concat $ ["/", show lang, "/", show entryId]

showLinks :: Maybe Int -> BlogLang -> Html
showLinks (Just i) lang = [shamlet|
  $if ((>) i 1)
    <div .navigation>
      <a href=#{nLink $ succ i} .uncoloured-link>#{backText lang}
      |
      <a href=#{nLink $ pred i} .uncoloured-link>#{nextText lang}
  $elseif ((<=) i 1)
    ^{showLinks Nothing lang}
|]
  where
   nLink page = T.concat ["/", show' lang, "/?page=", show' page]
showLinks Nothing lang = [shamlet|
<div .navigation>
  <a href=#{nLink} .uncoloured-link>#{backText lang}
|]
  where
   nLink = T.concat ["/", show' lang, "/?page=2"]

renderEntry :: Entry -> Html
renderEntry e@Entry{..} = [shamlet|
<article>
  <h2 .inline>
    #{title}
  <aside .date>
    #{pack $ formatTime defaultTimeLocale "%Y-%m-%d" edate}
  $if (isEntryMarkdown e)
    ^{renderEntryMarkdown btext}
    <p>^{renderEntryMarkdown $ mtext}
  $else
    ^{preEscapedToHtml $ btext}
    <p>^{preEscapedToHtml $ mtext}
<hr>
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
  <form action="/admin" method="POST">
   <p>Account ID
   <p><input type="text" style="font-size:2;" name="account" value="tazjin" readonly="1">
   <p>Passwort
   <p><input type="password" style="font-size:2;" name="password">
   <p><input alt="Anmelden" type="image" src="/static/signin.gif">
|]

adminIndex :: Text -> Html
adminIndex sUser = adminTemplate "Index" $ [shamlet|
<div style="float:center;">
 <form action="/admin/entry" method="POST">
  <table>
   <tr>
    <thead><td>Title:
    <td><input type="text" name="title">
   <tr>
    <thead><td>Language:
    <td><select name="lang">
     <option value="en">English
     <option value="de">Deutsch
   <tr>
    <thead><td>Text:
    <td>
     <textarea name="btext" cols="100" rows="15">
   <tr>
    <thead>
     <td style="vertical-align:top;">Read more:
    <td>
     <textarea name="mtext" cols="100" rows="15">
  <input type="hidden" name="author" value=#{sUser}>
  <input style="margin-left:20px;" type="submit" value="Submit">
 ^{adminFooter}
|]

adminFooter :: Html
adminFooter = [shamlet|
<a href="/">Front page
\ -- #
  <a href="/admin">New article
\ -- Entry list: #
  <a href="/admin/entrylist/en">EN
\ & #
<a href="/admin/entrylist/de">DE
|]

adminEntryList :: [Entry] -> Html
adminEntryList entries = adminTemplate "EntryList" $ [shamlet|
<div style="float: center;">
 <table>
  $forall entry <- entries
   <tr>
    <td><a href=#{append "/admin/entry/" (show' $ entryId entry)}>#{title entry}
    <td>#{formatPostDate $ edate entry}
|]
 where
  formatPostDate = formatTime defaultTimeLocale "[On %D at %H:%M]"

editPage :: Entry -> Html
editPage (Entry{..}) = adminTemplate "Index" $ [shamlet|
<div style="float:center;">
 <form action=#{append "/admin/entry/" (show' entryId)} method="POST">
  <table>
   <tr>
    <td>Title:
    <td>
     <input type="text" name="title" value=#{title}>
   <tr>
    <td style="vertical-align:top;">Text:
    <td>
     <textarea name="btext" cols="100" rows="15">#{btext}
   <tr>
    <td style="vertical-align:top;">Read more:
    <td>
     <textarea name="mtext" cols="100" rows="15">#{mtext}
  <input type="submit" style="margin-left:20px;" value="Submit">
  <p>^{adminFooter}
|]

showError :: BlogError -> BlogLang -> Html
showError NotFound l = blogTemplate l (T.append ": " $ notFoundTitle l) $ [shamlet|
<p>:(
<p>#{notFoundText l}
<hr>
|]
showError UnknownError l = blogTemplate l "" $ [shamlet|
<p>:(
<p>#{unknownErrorText l}
<hr>
|]
