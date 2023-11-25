{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Pages.Page_1 (demoPage, demoSearch, demoReply) where

import Control.Monad (mapM_, forM_)
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as Sa
import qualified Text.Blaze.Internal as I


import qualified Text.Blaze.Htmx as X
import qualified Text.Blaze.Htmx.WebSockets as X
import qualified System.Posix as A.A
import qualified Data.Aeson.Key as H

import Pages.MockData
    ( ImageLocator(PlainUrl, UnsplashLC),
      UserPrj(name, imageID),
      Project(users, url, title, category),
      projectList)


demoPage :: T.Text -> [ Project ] -> H.Html
demoPage aReason projects = 
  H.html H.! A.class_ "dark" $ do
    H.head $ do
      H.link H.! A.href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css" H.! A.rel "stylesheet"
      H.link H.! A.href "xstatic/pack_1.css" H.! A.rel "stylesheet"
      H.link H.! A.href "xstatic/pack_2.css" H.! A.rel "stylesheet"
      H.script H.! A.src "https://unpkg.com/htmx.org@1.9.9/dist/htmx.min.js" H.! crossOrigin "anonymous" H.! A.type_ "text/javascript" $ ""
      H.script H.! A.src "https://unpkg.com//htmx.org@1.9.9/dist/ext/ws.js" H.! crossOrigin "anonymous" H.! A.type_ "text/javascript" $ ""
    H.body H.! A.class_ "antialiased text-slate-500 dark:text-slate-400 dark:bg-slate-900" $ do
      H.div H.! A.style "color: red; background: rgb(115 120 128)" H.! A.class_ "lg:col-span-5 xl:col-span-6 flex flex-col" $
        H.div H.! A.class_ "relative z-10 rounded-xl bg-white shadow-xl ring-1 ring-slate-900/5 overflow-hidden my-auto xl:mt-18 dark:bg-slate-800" $
          H.div H.! A.class_ "container mx-auto p-4" $ do
            H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ H.toHtml aReason
            sectionD projects


sectionD :: [ Project ] -> H.Html
sectionD projects =
  let
    searchID = "search-results"
  in
  H.section $ do
    H.header H.! A.class_ "bg-white space-y-4 p-4 sm:px-8 sm:py-6 lg:p-4 xl:px-8 xl:py-6" $ do
      H.div H.! A.class_ "flex items-center justify-between" $ do
        H.h2 H.! A.class_ "font-semibold text-slate-900" $ H.toHtml ("Projects" :: T.Text)
        H.a H.! A.href "javascript:void()" H.! A.class_ "hover:bg-blue-400 group fl)ex items-center rounded-md bg-blue-500 text-white text-sm font-medium pl-2 pr-3 py-2 shadow-sm" $ do
          S.svg S.! Sa.width "20" H.! Sa.height "20" H.! Sa.fill "currentColor" H.! A.class_ "mr-2" $
            S.path S.! Sa.d "M10 5a1 1 0 0 1 1 1v3h3a1 1 0 1 1 0 2h-3v3a1 1 0 1 1-2 0v-3H6a1 1 0 1 1 0-2h3V6a1 1 0 0 1 1-1Z"
          H.toHtml ("New" :: T.Text)
      searchForm searchID
      searchIndicator
    projectsD searchID projects
    testWS


searchForm :: T.Text -> H.Html
searchForm searchID =
  let
    modSearchID = I.textValue $ "#" <> searchID
  in
  H.div H.! A.class_ "group relative" $ do
    S.svg S.! Sa.width "20" S.! Sa.height "20" S.! Sa.fill "currentColor" S.! 
          A.class_ "absolute left-3 top-1/2 -mt-2.5 text-slate-400 pointer-events-none group-focus-within:text-blue-500" $
      S.path S.! Sa.fillRule "evenodd" S.! Sa.clipRule "evenodd"
          S.! Sa.d "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z"
    H.input H.! A.class_ "form-control focus:ring-2 focus:ring-blue-500 focus:outline-none appearance-none w-full text-sm leading-6 text-white placeholder-slate-400 rounded-md py-2 pl-10 ring-1 ring-slate-200 shadow-sm"
      H.! A.type_ "search" H.! A.name "search" H.! A.placeholder "Filter projects..."
      H.! X.hxPost "/xsearch" H.! X.hxTrigger "keyup changed delay:500ms" H.! X.hxTarget modSearchID H.! hxIndicator ".htmx-indicator"


searchIndicator :: H.Html
searchIndicator =
  H.div H.! A.class_ "bg-white sm:px-8 sm:py-6 lg:p-4 xl:px-8 xl:py-6" $ do
    H.span H.! A.class_ "htmx-indicator" $ do
      H.img H.! A.src "xstatic/imgs/bars.svg"
      "Searching..."


demoSearch :: [ Project ] -> T.Text -> H.Html
demoSearch projects needle =
  case needle of
    "" -> ""
    _ -> mapM_ aProjectD projects


testWS :: H.Html
testWS = do
  H.div H.! X.hxExt "ws" H.! X.wsConnect "/stream" H.! A.class_ "bg-white sm:px-8 sm:py-6 lg:p-4 xl:px-8 xl:py-6" $ do
    H.form H.! A.id "form" H.! X.wsSend "" $ do
      H.input H.! A.type_ "text" H.! A.name "ws-message"
    H.table H.! A.class_ "flex flex-col space-y-4" $ do
      H.thead $ do
        H.tr $ do
          H.th "Reply"
      H.tbody H.! A.id "notifications" $ ""


demoReply :: T.Text -> H.Html
demoReply aReply =
  H.tbody H.! A.id "notifications" H.! X.hxSwapOob "beforeend" $ do
    H.tr $ do
      H.td H.! A.class_ "px-6 py-4 whitespace-nowrap text-sm text-slate-900" $ H.toHtml aReply


-- ("<div id=\"notifications\" hx-swap-oob=\"beforeend\">Reply to " <> hxMsg.wsMessage <> "</div>")

mockTableRow :: H.Html
mockTableRow = do
  H.td H.! A.class_ "px-6 py-4 whitespace-nowrap text-sm text-slate-900" $ H.toHtml ("John" :: T.Text)
  H.td H.! A.class_ "px-6 py-4 whitespace-nowrap text-sm text-slate-900" $ H.toHtml ("Laplace" :: T.Text)
  H.td H.! A.class_ "px-6 py-4 whitespace-nowrap text-sm text-slate-900" $ H.toHtml ("john@laplace.com" :: T.Text)


-- A components for showing projects
projectsD :: T.Text -> [ Project ] -> H.Html
projectsD searchID projects =
 let
    modSearchID = I.textValue $ searchID
  in
  H.ul H.! A.class_ "bg-slate-50 p-4 sm:px-8 sm:pt-6 sm:pb-8 lg:p-4 xl:px-8 xl:pt-6 xl:pb-8 grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-1 xl:grid-cols-2 gap-4 text-sm leading-6 dark:bg-slate-900/40 dark:ring-1 dark:ring-white/5"
    H.! A.id modSearchID $ ""


aProjectD :: Project -> H.Html
aProjectD aProj =
  H.li H.! A.class_ "group cursor-pointer rounded-md p-3 bg-white ring-1 ring-slate-200 shadow-sm hover:bg-blue-500 hover:ring-blue-500 hover:shadow-md dark:bg-slate-700 dark:ring-0 dark:highlight-white/10 dark:hover:bg-blue-500 hidden sm:block lg:hidden xl:block" $
    H.a H.! A.href (H.textValue aProj.url) H.! A.class_ "hover:bg-blue-500 hover:ring-blue-500 hover:shadow-md group rounded-md p-3 bg-white ring-1 ring-slate-200 shadow-sm" $
      H.dl H.! A.class_ "grid sm:block lg:grid xl:block grid-cols-2 grid-rows-2 items-center" $ do
        H.div $ do
          H.dt H.! A.class_ "sr-only" $ H.toHtml ("Title" :: T.Text)
          H.dd H.! A.class_ "group-hover:text-white font-semibold text-slate-900" $ H.toHtml aProj.title
        H.div $ do
          H.dt H.! A.class_ "sr-only" $ H.toHtml ("Category" :: T.Text)
          H.dd H.! A.class_ "group-hover:text-blue-200" $ H.toHtml aProj.category
        H.div H.! A.class_ "col-start-2 row-start-1 row-end-3 sm:mt-4 lg:mt-0 xl:mt-4" $ do
          H.dt H.! A.class_ "sr-only" $ H.toHtml ("Users" :: T.Text)
          H.dd H.! A.class_ "flex justify-end sm:justify-start lg:justify-end xl:justify-start -space-x-1.5" $
            forM_ aProj.users (\u -> H.img H.! A.src (unrefImage u.imageID) H.! A.alt (H.textValue u.name) H.! A.class_ "w-6 h-6 rounded-full bg-slate-100 ring-2 ring-white")
  where
  unrefImage aLocator =
    H.textValue $ case aLocator of
      UnsplashLC anID -> fullUnsplashUrl anID
      PlainUrl anUrl ->  anUrl
  
  fullUnsplashUrl anID =
    "https://images.unsplash.com/" <> anID <> "?auto=format&fit=facearea&facepad=2&w=48&h=48&q=80"


crossOrigin :: I.AttributeValue -> I.Attribute
crossOrigin =
  I.attribute "crossorigin" "crossorigin=\""

hxIndicator :: I.AttributeValue -> I.Attribute
hxIndicator =
  I.attribute "hx-indicator" "hx-indicator=\""
