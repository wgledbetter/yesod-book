module Main where

import qualified Data.Text as T
import Data.Time
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Yesod

-- Routing ---------------------------------------------------------------------

data RoutingStuff = RoutingStuff

-- A dynamic single route.
-- The route has a single user-defined element that can be one of A, B, or C.
data DynSingle = A | B | C deriving (Eq, Show, Read)

instance PathPiece DynSingle where
  -- basically just a smart constructor from Text
  fromPathPiece t = case T.head t of
    'A' -> Just A
    'B' -> Just B
    'C' -> Just C
    _ -> Nothing

  toPathPiece = T.pack . show

-- A dynamic multiple route.
-- The user can define multiple path components which will be parsed into a given type.
data DynMulti = DynMulti3 T.Text T.Text T.Text
  deriving (Eq, Show, Read)

instance PathMultiPiece DynMulti where
  -- Basically a smart constructor for DynMulti
  fromPathMultiPiece (x : y : z : []) = Just $ DynMulti3 x y z
  fromPathMultiPiece _ = Nothing

  toPathMultiPiece (DynMulti3 x y z) = [x, y, z]

mkYesod
  "RoutingStuff"
  [parseRoutes|
/ RootR GET
/static StaticR GET
/dyns/#DynSingle DynSingleR GET
/dynm/*DynMulti DynMultiR GET
/allReqs AllReqsR
|]

-- /specReq SpecReqR GET POST DELETE (getSpecReqR, postSpecReqR, deleteSpecReqR)

instance Yesod RoutingStuff where
  -- Set conditions for logging
  shouldLogIO RoutingStuff _ _ = return True

-- Logging example
getRootR :: Handler Html
getRootR = do
  $logDebug "reading current time"
  tm <- liftIO getCurrentTime
  defaultLayout
    [whamlet|
    <h1>#{show tm}
  |]

getStaticR :: Handler Html
getStaticR = do
  setMessage $ toHtml ("fromStatic" :: String)
  redirect $ DynMultiR $ DynMulti3 "you" "tried" "static"

-- and it makes sense that the handler takes the parsed dynamic route as an argument.
getDynSingleR :: DynSingle -> Handler Html
getDynSingleR ds = defaultLayout [whamlet|#{show ds}|]

getDynMultiR :: DynMulti -> Handler Value
getDynMultiR dm =
  let objL = ["content" .= (show dm)]
   in do
        mmsg <- getMessage
        (return . object) $
          maybe
            objL
            (\msg -> ("msg" .= renderHtml msg) : objL)
            mmsg

-- this one is called "handleXXX" because our route quasi-quote doesn't specify any specific request
handleAllReqsR :: Handler Html
handleAllReqsR = defaultLayout [whamlet|Hello World!|]

-- Handler return values -------------------------------------------------------

-- data TypedContent = TypedContent !ContentType !Content

-- class ToTypedContent a where
--   toTypedContent :: a -> TypedContent

-- Handler functions
-- - getYesod
--   - returns "foundation" value
--   - alternatively, use `ask`
-- - getUrlRender
-- - getRequest
--   - raw data about a request.
-- - waiRequest
--   - more raw data
-- - notFound
-- - permissionDenied
-- - invalidArgs
-- - sendFile
-- - setCookie
--   - won't become visible until next request
--   - could I setCookie and immediately force a redirect?
-- - redirect

-- NOTE: When we say the above are functions "inside" a monad, that really means
--   that those functions are their own instances of the monad, and that they're
--   useful to chain in sequence with more important bits of code. Like with put
--   inside State, put *is* a State, but it does something small and useful that
--   lends itself to usage in a larger >>=/>> sequence. All that to say that the
--   term "inside" doesn't imply a smaller scope.

-- Main ------------------------------------------------------------------------

main :: IO ()
main = warp 3000 RoutingStuff
