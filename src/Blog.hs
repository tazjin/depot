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
markdownCutoff = fromJust $ parseTimeM False defaultTimeLocale "%s" "1367149834"


-- blog CSS (admin is still static)
stylesheetSource = $(luciusFile "res/blog.lucius")
blogStyle = renderCssUrl undefined stylesheetSource

-- blog HTML
blogTemplate :: BlogLang -> Text -> Html -> Html
blogTemplate lang t_append body = [shamlet|
$doctype 5
  <head>
    <title>#{blogTitle lang t_append}
    <link rel="stylesheet" type="text/css" href="/static/bootstrap.css" media="all">
    <link rel="stylesheet" type="text/css" href="/static/blogv40.css" media="all">
    <link rel="alternate" type="application/rss+xml" title="RSS-Feed" href=#{rssUrl}>
    <meta http-equiv="content-type" content="text/html;charset=UTF-8">
  <body>
    <div #wrap>
      <div .header>
        <div .container >
          <div .row>
            <div .span12 .blogtitle>
              <a class="btitle" href="/">#{blogTitle lang empty}
          <div .row>
            <br>
            <div .span6>
              <span .contacts #cosx>^{contactInfo}
      <div .container>
        ^{body}
    <footer .footer>
      ^{showFooter lang $ pack version}
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
<div .container>
  <div .row>
    <div .span12 .righttext style="text-align: right;margin-right:-200px">
      Proudly made with #
      <a class="link" href="http://haskell.org">Haskell
      , #
      <a class="link" href="http://hackage.haskell.org/package/acid-state-0.6.3">Acid-State
      \ and without PHP, Java, Perl, MySQL and Python.
      <p>
        <a class="link" href=#{repoURL}>#{append "Version " v}
  <div .row .text-center>
    <div .span12>
      <span style="font-size:13px;font-family:Helvetica;">ಠ_ಠ
|]

isEntryMarkdown :: Entry -> Bool
isEntryMarkdown e = edate e > markdownCutoff

renderEntryMarkdown :: Text -> Html
renderEntryMarkdown = markdown def {msXssProtect = False} . fromStrict

renderEntries :: Bool -> [Entry] -> Text -> Maybe Html -> Html
renderEntries showAll entries topText footerLinks = [shamlet|
<div .row>
  <div .span12>
    <p>
      <span class="innerTitle">
        <b>#{topText}
$forall entry <- elist
  <div .row>
    <div .span2>
      <a href=#{linkElems entry}>
        <b>#{title entry}
        <br>
        <i>#{pack $ formatTime defaultTimeLocale "%Y-%m-%d" $ edate entry}
    <div .span10 .entry>
      $if (isEntryMarkdown entry)
        ^{renderEntryMarkdown $ append " " $ btext entry}
      $else
        ^{preEscapedToHtml $ append " " $ btext entry}
      $if ((/=) (mtext entry) empty)
        <p>
          <a .readmore href=#{linkElems entry}>#{readMore $ lang entry}
      $else
        <br>&nbsp;
$maybe links <- footerLinks
  ^{links}
|]
  where
   elist = if' showAll entries (take 6 entries)
   linkElems Entry{..} = concat $ intersperse' "/" [show lang, show entryId]

showLinks :: Maybe Int -> BlogLang -> Html
showLinks (Just i) lang = [shamlet|
  $if ((>) i 1)
    <div .row .text-center>
      <div .span12>
        <a href=#{nLink $ succ i}>#{backText lang}
        \ -- #
        <a href=#{nLink $ pred i}>#{nextText lang}
  $elseif ((<=) i 1)
    ^{showLinks Nothing lang}
|]
  where
   nLink page = T.concat ["/", show' lang, "/?page=", show' page]
showLinks Nothing lang = [shamlet|
<div .row .text-center>
  <div .span12>
    <a href=#{nLink}>#{backText lang}
|]
  where
   nLink = T.concat ["/", show' lang, "/?page=2"]

renderEntry :: Entry -> Html
renderEntry e@Entry{..} = [shamlet|
<div .row .pusher>
  <div .span9>
    <span .boldify>#{title}
  <div .span3>
    <span .righttext><i>#{woText}</i>
<div .row .innerContainer>
  <div .span10>
    <article>
      $if (isEntryMarkdown e)
        ^{renderEntryMarkdown btext}
        <p>^{renderEntryMarkdown $ mtext}
      $else
        ^{preEscapedToHtml $ btext}
        <p>^{preEscapedToHtml $ mtext}
|]
  where
   woText = flip T.append author $ T.pack $ formatTime defaultTimeLocale (eTimeFormat lang) edate

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
<div .row .text-center>
  <div .span12  .notFoundFace>:(
<div .row .text-center>
  <div .span12 .notFoundText>
    #{notFoundText l}
|]

