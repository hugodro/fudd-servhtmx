module Pages.MockData where

import qualified Data.Text as T


data Project = Project {
    url :: T.Text
    , title :: T.Text
    , category :: T.Text
    , users :: [ UserPrj ]
  }

data UserPrj = UserPrj {
    name :: T.Text
    , imageID :: ImageLocator
  }


data ImageLocator =
  UnsplashLC T.Text
  | PlainUrl T.Text


projectList :: [Project]
projectList = [
  Project {
    url = "proj_1"
    , title ="First Project"
    , category = "Testing"
    , users = [
      UserPrj "Joe A" (UnsplashLC "photo-1531123897727-8f129e1688ce")
      , UserPrj "Jane B" (UnsplashLC "photo-1494790108377-be9c29b29330")
    ]
  }
  , Project {
    url = "proj_2"
    , title ="Second Project"
    , category = "More testing"
    , users = [
      UserPrj "Achmed A" (UnsplashLC "photo-1494790108377-be9c29b29330")
      , UserPrj "Simona B" (UnsplashLC "photo-1506794778202-cad84cf45f1d")
      , UserPrj "Jonh C" (PlainUrl "https://allinfra.com/_next/image?url=%2Fteam-gallery%2FChi-Hang-Ho.webp&w=2048&q=75")
    ]
  }
 ]

