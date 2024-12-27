-- I want a webpage to be fully loaded but have the JS update the HTML with websocket data.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson as A
import Data.Conduit
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time
import GHC.Generics
import System.Random (randomRIO)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Yesod
import Yesod.WebSockets

-- General ---------------------------------------------------------------------

newtype MicroSeconds = MicroSeconds Int deriving (Eq, Show)

data Point = Point {x :: Double, y :: Double}
  deriving (Eq, Show, Generic)

instance ToJSON Point

-- Foundation Stuff ------------------------------------------------------------

port :: Int
port = 3000

data Ex11 = Ex11

mkYesod
  "Ex11"
  [parseRoutes|
/ HomeR GET
/plot PlotlyR GET
|]

instance Yesod Ex11

-- Route Handler ---------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = do
  -- Run server-side websocket code
  webSockets $ runConduit (timeSource (MicroSeconds 100) .| sinkWSText)

  -- Send HTML with socket-interactive JS
  defaultLayout $ toWidget $ wsjs $ "ws://localhost:" <> show port <> "/"

getPlotlyR :: Handler Html
getPlotlyR = do
  webSockets $ runConduit (dataSource (MicroSeconds 100_000) .| sinkWSText)

  defaultLayout $ do
    addScriptRemote "https://cdn.plot.ly/plotly-2.35.2.min.js"
    -- TODO: Use Yesod's built-in url stuff for this.
    toWidget $ wsjs' $ "ws://localhost:" <> show port <> "/plot"

-- Data Source -----------------------------------------------------------------

timeSource :: (MonadIO m) => MicroSeconds -> ConduitT () TL.Text m ()
timeSource (MicroSeconds rate) = forever $ do
  now <- liftIO getCurrentTime
  yield $ TL.pack $ show now
  liftIO $ threadDelay rate

dataSource :: (MonadIO m) => MicroSeconds -> ConduitT () TL.Text m ()
dataSource (MicroSeconds rate) = forever $ do
  xV <- liftIO (randomRIO (-1, 1) :: IO Double)
  yV <- liftIO (randomRIO (-1, 1) :: IO Double)
  yield $ TL.decodeUtf8 $ A.encode $ Point {x = xV, y = yV}
  liftIO $ threadDelay rate

-- WebSocket JS ----------------------------------------------------------------

wsjs :: String -> JavascriptUrl a
wsjs url =
  let heading = renderHtml [shamlet|<h1 #ws>WS Data|]
   in [julius|
var conn = new WebSocket(#{url});

conn.onopen = function () {
  document.write(#{heading});
};

conn.onmessage = function (e) {
  document.getElementById("ws").textContent = e.data;
};

conn.onclose = function () {
  document.getElementById("ws").textContent = "WS Closed.";
};
|]

wsjs' :: String -> JavascriptUrl a
wsjs' url =
  let plotlyHolder =
        renderHtml
          [shamlet|
<body>
  <div #plt>
|]
   in [julius|
var trace = {
  x: [0],
  y: [0],
  mode: 'markers',
  type: 'scatter'
};

var conn = new WebSocket(#{url});

conn.onopen = function () {
  document.write(#{plotlyHolder});
  Plotly.newPlot('plt', [trace]);
};

conn.onmessage = function (e) {
  const mj = JSON.parse(e.data);
  trace.x.push(mj.x);
  trace.y.push(mj.y);
  Plotly.redraw('plt');
};

conn.onclose = function () {
  const e = document.getElementById("plt");
  e.parentNode.parentNode.removeChild(e.parentNode);
};
|]

-- Main ------------------------------------------------------------------------

main :: IO ()
main = warp port Ex11
