-- I want a webpage to be fully loaded but have the JS update the HTML with websocket data.

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Conduit
import qualified Data.Text.Lazy as TL
import Data.Time
import Text.Blaze.Html.Renderer.String (renderHtml)
import Yesod
import Yesod.WebSockets

-- General ---------------------------------------------------------------------

newtype MilliSeconds = MilliSeconds Int deriving (Eq, Show)

-- Foundation Stuff ------------------------------------------------------------

port :: Int
port = 3000

data Ex11 = Ex11

mkYesod
  "Ex11"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod Ex11

-- Route Handler ---------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = do
  -- Run server-side websocket code
  webSockets $ runConduit (timeSource (MilliSeconds 100) .| sinkWSText)

  -- Send HTML with socket-interactive JS
  defaultLayout $ toWidget $ wsjs $ "ws://localhost:" <> show port <> "/"

-- Data Source -----------------------------------------------------------------

timeSource :: (MonadIO m) => MilliSeconds -> ConduitT () TL.Text m ()
timeSource (MilliSeconds rate) = forever $ do
  now <- liftIO getCurrentTime
  yield $ TL.pack $ show now
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

-- Main ------------------------------------------------------------------------

main :: IO ()
main = warp port Ex11
