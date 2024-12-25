{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Network.HTTP.Types (encodePath)
import Yesod

data Sample = Sample

mkYesod
  "Sample"
  [parseRoutes|
/ RootR GET
/foo FooR GET
/err ErrR GET
|]

instance Yesod Sample where
  -- ApprootMaster: Use "foundation value" (Sample constructor) to determine root
  -- ApprootRequest: Use web request contents.
  -- approot = ApprootStatic "http://static.example.com/wiki"

  joinPath _ ar pieces' qs' = fromText ar `mappend` encodePath pieces qs
    where
      qs = map (\(x, y) -> (TE.encodeUtf8 x, go y)) qs'

      go "" = Nothing
      go x = Just $ TE.encodeUtf8 x

      pieces = pieces' ++ [""]

  cleanPath _ [] = Right []
  cleanPath _ s
    | dropWhile (not . T.null) s == [""] = Right $ init s
    | otherwise = Left $ filter (not . T.null) s

  defaultLayout widget = do
    pc <-
      widgetToPageContent $ do
        widget
        -- make some changes/additions to the given widget before finalizing it.
        toWidget
          [lucius|
          body { font-family: verdana; }
          head { font-family: helvetica; }
          div { font-family: verdana; color: red; }
        |]

    -- Get (and clear) the message in a user session.
    mmsg <- getMessage

    withUrlRenderer
      [hamlet|
        $doctype 5
        <html>
          <head>
            <title>#{pageTitle pc}
            <meta charset=utf-8>
            ^{pageHead pc}
          <body>
            $maybe msg <- mmsg
              <div #message>#{msg}
            <article>
              ^{pageBody pc}
      |]

  errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
    setTitle "whoopsy i made an oopsie"
    toWidget
      [hamlet|
<h1>unfound
<p>sowwy
|]
  errorHandler other = defaultErrorHandler other

getRootR :: Handler Html
getRootR = do
  -- NOTE: I can use liftIO to run time-sensitive actions like reading TVars.
  now <- liftIO getCurrentTime
  -- Set message in user session
  setMessage $ toHtml $ "You previously visited at: " <> show now
  defaultLayout
    [whamlet|
<p>
  <a href=@{RootR}>RootR
<p>
  <a href=@{FooR}>FooR
<p>
  <a href=@{ErrR}>ErrR
|]

getFooR :: Handler Html
getFooR = getRootR

getErrR :: Handler ()
getErrR = notFound

-- Main ------------------------------------------------------------------------

main :: IO ()
main = warp 3000 Sample
