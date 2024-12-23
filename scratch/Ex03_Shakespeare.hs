{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Blaze.ByteString.Builder (toByteString)
import Control.Arrow (second)
import Data.Char (toLower)
import Data.List (sort)
import Data.Text (Text, append, concat, pack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TLIO
import Network.HTTP.Types (renderQueryText)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet
  ( HtmlUrl,
    HtmlUrlI18n,
    hamlet,
    ihamlet,
    shamlet,
  )
import Text.Internal.Css (Css, Mixin)
import Text.Lucius
  ( CssUrl,
    lucius,
    luciusFile,
    luciusMixin,
    renderCss,
  )

-- Ex 01 -----------------------------------------------------------------------

data Person = Person {name :: String, age :: Int}

-- "Variable Interpolation" uses #{}
main1 :: IO ()
main1 =
  putStrLn $
    renderHtml
      [shamlet|
<p>Hi #{name person}. You are #{show $ age person}.
<p>
    Arbitrary haskell in an "interpolation": #
    <b>#{sort $ map toLower (name person)}
<p>Aren't you nervous for when you turn #{show (5 + age person)}?
|]
  where
    person = Person "Rumplestiltskin" 485

-- Ex 02 -----------------------------------------------------------------------

data MyRoute = Home

render2 :: MyRoute -> [(Text, Text)] -> Text
render2 Home _ = "/home"

-- Url Interpolation: @{}
footer :: HtmlUrl MyRoute
footer =
  [hamlet|
<footer>
    Return to #
    <a href=@{Home}>Homepage
    .
|]

-- HTML/Template Interpolation: ^{}
main2 :: IO ()
main2 =
  putStrLn $
    renderHtml $
      [hamlet|
<body>
    <p>This is my page.
    ^{footer}
|]
        render2

-- Ex 03 -----------------------------------------------------------------------

data AnotherRoute = SomePage

render3 :: AnotherRoute -> [(Text, Text)] -> Text
render3 SomePage params =
  "/home"
    `append` decodeUtf8
      ( toByteString $
          renderQueryText
            True
            (map (second Just) params)
      )

-- Url interpolation with params: @?{}
-- Useful for linking to a page with certain predetermined arguments
main3 :: IO ()
main3 = do
  let pg = 2 :: Int
      -- Route and params to page minus one
      prv = (SomePage, [("page", pack $ show $ pg - 1)])
      -- Route and params to page plus one
      nxt = (SomePage, [("page", pack $ show $ pg + 1)])
  putStrLn $
    renderHtml $
      [hamlet|
<p>
    Welcome to page #{pg}.
    <a href=@?{prv}>Previous
    <a href=@?{nxt}>Next
|]
        render3

-- Attributes
-- #whatever -> id="whatever"
-- .stuff -> class="stuff"
-- :False:yeet -> _
-- :True:yeet -> yeet
-- :True:.aThing -> class="aThing"

-- * {[("k","v"), ("arbi","trary")]} -> k="v" arbi="trary"

-- Conditionals

-- $if pred
--   <p>ness

-- $elseif pred'
--   <p>u

-- $else
--   <a href="crap">

-- Maybe

-- $maybe val <- mbVal
--   <p>v = #{val}

-- $nothing
--   <p>empty

-- Forall

-- $if null myList
--   <p>Nuttin

-- $else
--   <ul>
--     $forall itm <- myList
--       <li>#{itm}

-- Case

-- With

-- Lucius Ex 01 ----------------------------------------------------------------

renderL1 :: a
renderL1 = undefined

transition :: String -> Mixin
transition val =
  [luciusMixin|
  -webkit-transition: #{val};
  -moz-transition: #{val};
  -ms-transition: #{val};
  -o-transition: #{val};
  transition: #{val};
|]

myCSS :: p -> Css
myCSS =
  [lucius|
  .some-class {
    ^{transition "all 4s ease"}
  }
|]

main4 :: IO ()
main4 = TLIO.putStrLn $ renderCss $ myCSS renderL1

-- Ex 05 -----------------------------------------------------------------------

data Route5 = Home5 | Time | Stylesheet

render5 :: Route5 -> [(Text, Text)] -> Text
render5 Home5 _ = "/home"
render5 Time _ = "/time"
render5 Stylesheet _ = "/style.css"

template5 :: Text -> HtmlUrl Route5
template5 title =
  [hamlet|
$doctype 5
<html>
  <head>
    <title>#{title}
    <link rel=stylesheet href=@{Stylesheet}>
  <body>
    <h1>#{title}
|]

main5 :: IO ()
main5 = putStrLn $ renderHtml $ template5 "Some Title" render5

-- Ex 06 -----------------------------------------------------------------------

template6 :: CssUrl Route5
template6 = $(luciusFile "ex3.lucius")

main6 :: IO ()
main6 = TLIO.putStrLn $ renderCss $ template6 render5

-- Ex 07: Internationalization -------------------------------------------------

data Msg = Hello | Apples Int

renderEnglish :: Msg -> Text
renderEnglish Hello = "Hello"
renderEnglish (Apples 0) = "You did not buy any apples."
renderEnglish (Apples 1) = "You bought 1 apple."
renderEnglish (Apples a) = Data.Text.concat ["You bought ", pack $ show a, " apples."]

-- The _{} "interpolates" via some translator function
template7 :: Int -> HtmlUrlI18n Msg Route5
template7 count =
  [ihamlet|
$doctype 5
<html>
  <head>
    <title>i18n
  <body>
    <h1>_{Hello}
    <p>_{Apples count}
|]

main7 :: IO ()
main7 = putStrLn $ renderHtml $ (template7 88) (toHtml . renderEnglish) render5

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  let vizBreak = putStrLn "\n------------------------------------------------\n"
  main1
  vizBreak
  main2
  vizBreak
  main3
  vizBreak
  main4
  vizBreak
  main5
  vizBreak
  main6
  vizBreak
  main7
