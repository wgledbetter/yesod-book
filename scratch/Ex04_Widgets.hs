{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod

data App = App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
  defaultLayout = myLayout

-- So, is (HandlerFor App) the widget? That's definitely the monad, right?
-- also, nobody calls getHomeR. How is that registered?
--   it's probably called by some part of the TH splice above, and you have to write a getXXX function for each route.
getHomeR :: HandlerFor App Html
getHomeR = defaultLayout $ do
  setTitle "My Page Title"
  toWidget [lucius| h1 { color: green; } |]
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
  toWidget
    [julius|
            $(function() {
                $("h1").click(function(){
                    alert("You clicked on the heading!");
                });
            });
        |]
  toWidgetHead
    [hamlet|
            <meta name=keywords content="some sample keywords">
        |]
  toWidget
    [hamlet|
            <h1>Here's one way of including content
        |]
  [whamlet|<h2>Here's another |]
  toWidgetBody
    [julius|
            alert("This is included in the body itself");
        |]

-- Widget is also a Monoid, so you can use mconcat
-- type Widget = WidgetFor App ()
-- WidgetFor is a monad transformer

myLayout :: Widget -> Handler Html
myLayout widget = do
  pc <-
    widgetToPageContent $ do
      widget
      -- make some changes/additions to the given widget before finalizing it.
      toWidget [lucius| body {font-family: verdana }|]

  withUrlRenderer
    [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{pageTitle pc}
          <meta charset=utf-8>
          ^{pageHead pc}
        <body>
          <article>
            ^{pageBody pc}
    |]

main :: IO ()
main = warp 3000 App
