{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Pages.Page_1 (demoPage, demoSearch, demoReply) where

import Prelude hiding (head, div, id, span)

import Control.Monad (mapM_, forM_)
import qualified Data.Text as T

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as Sa
import qualified Text.Blaze.Internal as I


import qualified Text.Blaze.Htmx as X
import qualified Text.Blaze.Htmx.WebSockets as X
import qualified System.Posix as A
import qualified Data.Aeson.Key as H

import Pages.MockData
    ( ImageLocator(PlainUrl, UnsplashLC),
      UserPrj(name, imageID),
      Project(users, url, title, category),
      projectList)


demoPage :: T.Text -> [ Project ] -> Html
demoPage aReason projects = 
  html ! class_ "dark" $ do
    head $ do
      link ! href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css" ! rel "stylesheet"
      link ! href "xstatic/pack_1.css" ! rel "stylesheet"
      link ! href "xstatic/pack_2.css" ! rel "stylesheet"
      script ! src "https://unpkg.com/htmx.org@1.9.9/dist/htmx.min.js" ! crossOrigin "anonymous" ! type_ "text/javascript" $ ""
      script ! src "https://unpkg.com//htmx.org@1.9.9/dist/ext/wjs" ! crossOrigin "anonymous" ! type_ "text/javascript" $ ""
    body ! class_ "antialiased text-slate-500 dark:text-slate-400 dark:bg-slate-900" $
      div ! A.style "color: red; background: rgb(115 120 128)" ! class_ "lg:col-span-5 xl:col-span-6 flex flex-col" $
        div ! class_ "relative z-10 rounded-xl bg-white shadow-xl ring-1 ring-slate-900/5 overflow-hidden my-auto xl:mt-18 dark:bg-slate-800" $
          div ! class_ "container mx-auto p-4" $ do
            h1 ! class_ "text-2xl font-bold mb-4" $ toHtml aReason
            sectionD projects


demoSearch :: [ Project ] -> T.Text -> Html
demoSearch projects needle =
  case needle of
    "" -> ""
    _ -> mapM_ aProjectD projects


demoReply :: T.Text -> Html
demoReply aReply =
  tbody ! id "notifications" ! X.hxSwapOob "beforeend" $ do
    tr $ do
      td ! class_ "px-6 py-4 whitespace-nowrap text-sm text-slate-900" $ toHtml aReply


sectionD :: [ Project ] -> Html
sectionD projects =
  let
    searchID = "search-results"
  in
  section $ do
    header ! class_ "bg-white space-y-4 p-4 sm:px-8 sm:py-6 lg:p-4 xl:px-8 xl:py-6" $ do
      div ! class_ "flex items-center justify-between" $ do
        h2 ! class_ "font-semibold text-slate-900" $ toHtml ("Projects" :: T.Text)
        a ! href "javascript:void()" ! class_ "hover:bg-blue-400 group fl)ex items-center rounded-md bg-blue-500 text-white text-sm font-medium pl-2 pr-3 py-2 shadow-sm" $ do
          S.svg ! Sa.width "20" ! Sa.height "20" ! Sa.fill "currentColor" ! class_ "mr-2" $
            S.path ! Sa.d "M10 5a1 1 0 0 1 1 1v3h3a1 1 0 1 1 0 2h-3v3a1 1 0 1 1-2 0v-3H6a1 1 0 1 1 0-2h3V6a1 1 0 0 1 1-1Z"
          toHtml ("New" :: T.Text)
      searchForm searchID
      searchIndicator
    projectsD searchID projects
    testWS


searchForm :: T.Text -> Html
searchForm searchID =
  let
    modSearchID = I.textValue $ "#" <> searchID
  in
  div ! class_ "group relative" $ do
    S.svg ! Sa.width "20" ! Sa.height "20" ! Sa.fill "currentColor" ! 
          class_ "absolute left-3 top-1/2 -mt-2.5 text-slate-400 pointer-events-none group-focus-within:text-blue-500" $
      S.path ! Sa.fillRule "evenodd" ! Sa.clipRule "evenodd"
          ! Sa.d "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z"
    input ! class_ "form-control focus:ring-2 focus:ring-blue-500 focus:outline-none appearance-none w-full text-sm leading-6 text-white placeholder-slate-400 rounded-md py-2 pl-10 ring-1 ring-slate-200 shadow-sm"
      ! type_ "search" ! A.name "search" ! placeholder "Filter project.."
      ! X.hxPost "/xsearch" ! X.hxTrigger "keyup changed delay:500ms" ! X.hxTarget modSearchID ! hxIndicator ".htmx-indicator"


searchIndicator :: Html
searchIndicator =
  div ! class_ "bg-white sm:px-8 sm:py-6 lg:p-4 xl:px-8 xl:py-6" $ do
    H.span ! class_ "htmx-indicator" $ do
      img ! src "xstatic/imgs/barsvg"
      "Searching..."


testWS :: Html
testWS = do
  div ! X.hxExt "ws" ! X.wsConnect "/stream" ! class_ "bg-white sm:px-8 sm:py-6 lg:p-4 xl:px-8 xl:py-6" $ do
    H.form ! id "form" ! X.wsSend "" $ do
      input ! type_ "text" ! A.name "ws-message"
    table ! class_ "flex flex-col space-y-4" $ do
      thead $ do
        tr $ do
          th "Reply"
      tbody ! id "notifications" $ ""


-- ("<div id=\"notifications\" hx-swap-oob=\"beforeend\">Reply to " <> hxMsg.wsMessage <> "</div>")

mockTableRow :: Html
mockTableRow = do
  td ! class_ "px-6 py-4 whitespace-nowrap text-sm text-slate-900" $ toHtml ("John" :: T.Text)
  td ! class_ "px-6 py-4 whitespace-nowrap text-sm text-slate-900" $ toHtml ("Laplace" :: T.Text)
  td ! class_ "px-6 py-4 whitespace-nowrap text-sm text-slate-900" $ toHtml ("john@laplace.com" :: T.Text)


-- A components for showing projects
projectsD :: T.Text -> [ Project ] -> Html
projectsD searchID projects =
 let
    modSearchID = I.textValue $ searchID
  in
  ul ! class_ "bg-slate-50 p-4 sm:px-8 sm:pt-6 sm:pb-8 lg:p-4 xl:px-8 xl:pt-6 xl:pb-8 grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-1 xl:grid-cols-2 gap-4 text-sm leading-6 dark:bg-slate-900/40 dark:ring-1 dark:ring-white/5"
    ! id modSearchID $ ""


aProjectD :: Project -> Html
aProjectD aProj =
  li ! class_ "group cursor-pointer rounded-md p-3 bg-white ring-1 ring-slate-200 shadow-sm hover:bg-blue-500 hover:ring-blue-500 hover:shadow-md dark:bg-slate-700 dark:ring-0 dark:highlight-white/10 dark:hover:bg-blue-500 hidden sm:block lg:hidden xl:block" $
    a ! href (textValue aProj.url) ! class_ "hover:bg-blue-500 hover:ring-blue-500 hover:shadow-md group rounded-md p-3 bg-white ring-1 ring-slate-200 shadow-sm" $
      dl ! class_ "grid sm:block lg:grid xl:block grid-cols-2 grid-rows-2 items-center" $ do
        div $ do
          dt ! class_ "sr-only" $ toHtml ("Title" :: T.Text)
          dd ! class_ "group-hover:text-white font-semibold text-slate-900" $ toHtml aProj.title
        div $ do
          dt ! class_ "sr-only" $ toHtml ("Category" :: T.Text)
          dd ! class_ "group-hover:text-blue-200" $ toHtml aProj.category
        div ! class_ "col-start-2 row-start-1 row-end-3 sm:mt-4 lg:mt-0 xl:mt-4" $ do
          dt ! class_ "sr-only" $ toHtml ("Users" :: T.Text)
          dd ! class_ "flex justify-end sm:justify-start lg:justify-end xl:justify-start -space-x-1.5" $
            forM_ aProj.users (\u -> img ! src (unrefImage u.imageID) ! alt (textValue u.name) ! class_ "w-6 h-6 rounded-full bg-slate-100 ring-2 ring-white")
  where
  unrefImage aLocator =
    textValue $ case aLocator of
      UnsplashLC anID -> fullUnsplashUrl anID
      PlainUrl anUrl ->  anUrl
  
  fullUnsplashUrl anID =
    "https://imageunsplascom/" <> anID <> "?auto=format&fit=facearea&facepad=2&w=48&h=48&q=80"


crossOrigin :: I.AttributeValue -> I.Attribute
crossOrigin =
  I.attribute "crossorigin" "crossorigin=\""

hxIndicator :: I.AttributeValue -> I.Attribute
hxIndicator =
  I.attribute "hx-indicator" "hx-indicator=\""
