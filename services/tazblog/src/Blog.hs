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
import Data.Text (Text, empty, pack)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Time
import Locales
import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet
import Text.Markdown

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

-- |After this date all entries are Markdown
markdownCutoff :: Day
markdownCutoff = fromGregorian 2013 04 28

blogTemplate :: BlogLang -> Text -> Html -> Html
blogTemplate lang t_append body =
  [shamlet|
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
showFooter =
  [shamlet|
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
    linkElems Entry {..} = concat $ ["/", show lang, "/", show entryId]

showLinks :: Maybe Int -> BlogLang -> Html
showLinks (Just i) lang =
  [shamlet|
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
showLinks Nothing lang =
  [shamlet|
<div .navigation>
  <a href=#{nLink} .uncoloured-link>#{backText lang}
|]
  where
    nLink = T.concat ["/", show' lang, "/?page=2"]

renderEntry :: Entry -> Html
renderEntry e@Entry {..} =
  [shamlet|
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

showError :: BlogError -> BlogLang -> Html
showError NotFound l =
  blogTemplate l (T.append ": " $ notFoundTitle l)
    $ [shamlet|
<p>:(
<p>#{notFoundText l}
<hr>
|]
showError UnknownError l =
  blogTemplate l ""
    $ [shamlet|
<p>:(
<p>#{unknownErrorText l}
<hr>
|]
