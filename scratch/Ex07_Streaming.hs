{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import Yesod

data App = App
  { jobs :: TVar (IntMap (TChan (Maybe Text))),
    nextJob :: TVar Int
  }

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET POST
/view-progress/#Int ViewProgressR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "PubSub example"
  [whamlet|
        <form method=post>
            <button>Start new background job
    |]

postHomeR :: Handler ()
postHomeR = do
  App {..} <- getYesod
  (jobId, chan) <- liftIO $ atomically $ do
    jobId <- readTVar nextJob
    writeTVar nextJob $! jobId + 1
    chan <- newBroadcastTChan
    m <- readTVar jobs
    writeTVar jobs $ IntMap.insert jobId chan m
    return (jobId, chan)
  _ <- liftIO $ forkIO $ do
    threadDelay 5_000_000
    atomically $ writeTChan chan $ Just "Did something\n"
    threadDelay 5_000_000
    atomically $ writeTChan chan $ Just "Did something else\n"
    threadDelay 5_000_000
    atomically $ do
      writeTChan chan $ Just "All done\n"
      writeTChan chan Nothing
      m <- readTVar jobs
      writeTVar jobs $ IntMap.delete jobId m
  redirect $ ViewProgressR jobId

-- This function is able to "stream" multiple responses to the client so the intermediate product can be displayed.
-- It seems additive though. I can't replace something old with something new, only concatenate additional info.
getViewProgressR :: Int -> Handler TypedContent
getViewProgressR jobId = do
  App {..} <- getYesod
  mchan <- liftIO $ atomically $ do
    m <- readTVar jobs
    case IntMap.lookup jobId m of
      Nothing -> return Nothing
      Just chan -> fmap Just $ dupTChan chan
  case mchan of
    Nothing -> notFound
    Just chan -> respondSource typeHtml $ do
      let loop = do
            mtext <- liftIO $ atomically $ readTChan chan
            case mtext of
              Nothing -> return ()
              Just text -> do
                sendChunkHtml [shamlet|<h1>#{text}|]
                sendFlush
                loop
      loop

main :: IO ()
main = do
  jobs <- newTVarIO IntMap.empty
  nextJob <- newTVarIO 1
  warp 3000 App {..}
