{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Conduit
import qualified Data.Conduit.List
import qualified Data.Text.Lazy as TL
import Data.Time
import Yesod
import Yesod.WebSockets

port :: Int
port = 3000

data App = App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod App -- this has to come after mkYesod

timeSource :: (MonadIO m) => ConduitT () TL.Text m ()
timeSource = forever $ do
  now <- liftIO getCurrentTime
  yield $ TL.pack $ show now
  liftIO $ threadDelay 5_000_000

getHomeR :: Handler Html
getHomeR = do
  webSockets $
    race_
      -- Echo recieved messages in all caps
      (runConduit (sourceWS .| Data.Conduit.List.map TL.toUpper .| sinkWSText))
      -- Send time every 5 seconds
      (runConduit (timeSource .| sinkWSText))
  let wsUrl = "ws://localhost:" <> show port <> "/"
  defaultLayout $
    toWidget
      [julius|
                var conn = new WebSocket(#{wsUrl});
                conn.onopen = function() {
                    document.write("<p>open!</p>");
                    document.write("<button id=button>Send another message</button>")
                    document.getElementById("button").addEventListener("click", function(){
                        var msg = prompt("Enter a message for the server");
                        conn.send(msg);
                    });
                    conn.send("hello world");
                };
                conn.onmessage = function(e) {
                    document.write("<p>" + e.data + "</p>");
                };
                conn.onclose = function () {
                    document.write("<p>Connection Closed</p>");
                };
            |]

main :: IO ()
main = warp port App
