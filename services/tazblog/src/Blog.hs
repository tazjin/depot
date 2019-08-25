{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Blog where

import BlogStore
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Time
import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet
import Text.Markdown

blogTitle :: Text = "tazjin's blog"

repoURL :: Text = "https://bitbucket.org/tazjin/tazblog-haskell"

mailTo :: Text = "mailto:mail@tazj.in"

twitter :: Text = "https://twitter.com/tazjin"

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

-- |After this date all entries are Markdown
markdownCutoff :: Day
markdownCutoff = fromGregorian 2013 04 28

blogTemplate :: Text -> Html -> Html
blogTemplate t_append body =
  [shamlet|
$doctype 5
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content=#{blogTitle}#{t_append}>
    <link rel="stylesheet" type="text/css" href="/static/blog.css" media="all">
    <link rel="alternate" type="application/rss+xml" title="RSS-Feed" href="/rss.xml">
    <title>#{blogTitle}#{t_append}
  <body>
    <header>
      <h1>
        <a href="/" .unstyled-link>#{blogTitle}
      <hr>
    ^{body}
    ^{showFooter}
|]

showFooter :: Html
showFooter =
  [shamlet|
<footer>
  <p .footer>Served without any dynamic languages.
  <p .footer>
    <a href=#{repoURL} .uncoloured-link>
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

renderEntries :: [Entry] -> Maybe Html -> Html
renderEntries entries pageLinks =
  [shamlet|
$forall entry <- entries
  <article>
    <h2 .inline>
      <a href=#{linkElems entry} .unstyled-link>
        #{title entry}
    <aside .date>
      #{pack $ formatTime defaultTimeLocale "%Y-%m-%d" $ edate entry}
    $if (isEntryMarkdown entry)
      ^{renderEntryMarkdown $ text entry}
    $else
      ^{preEscapedToHtml $ text entry}
  <hr>
$maybe links <- pageLinks
  ^{links}
|]
  where
    linkElems Entry {..} = concat $ ["/", show entryId]

showLinks :: Maybe Int -> Html
showLinks (Just i) =
  [shamlet|
  $if ((>) i 1)
    <div .navigation>
      <a href=#{nLink $ succ i} .uncoloured-link>Earlier
      |
      <a href=#{nLink $ pred i} .uncoloured-link>Later
  $elseif ((<=) i 1)
    ^{showLinks Nothing}
|]
  where
    nLink page = T.concat ["/?page=", show' page]
showLinks Nothing =
  [shamlet|
<div .navigation>
  <a href="/?page=2" .uncoloured-link>Earlier
|]

renderEntry :: Entry -> Html
renderEntry e@Entry {..} =
  [shamlet|
<article>
  <h2 .inline>
    #{title}
  <aside .date>
    #{pack $ formatTime defaultTimeLocale "%Y-%m-%d" edate}
  $if (isEntryMarkdown e)
    ^{renderEntryMarkdown text}
  $else
    ^{preEscapedToHtml $ text}
<hr>
|]

showError :: Text -> Text -> Html
showError title err =
  blogTemplate (": " <> title)
    $ [shamlet|
<p>:(
<p>#{err}
<hr>
|]
