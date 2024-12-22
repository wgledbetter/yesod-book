{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main, resourcesApp, Widget, Handler) where

import Yesod

data App = App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod App

-- Note the type differentce of "Value" vs. "Html" in the previous examples.
getHomeR :: HandlerFor App Value
getHomeR = return $ object ["msg" .= ("Hello World" :: String)]

main :: IO ()
main = warp 3000 App
