{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WebServer.CorsPolicy where

import Data.Aeson (FromJSON, ToJSON)
import Data.CaseInsensitive as CI
import Data.List (elem, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Types.Header (hOrigin)
import Network.Wai
import Network.Wai.Middleware.Cors


data CorsConfig = CorsConfig {
   allowedOrigins :: [ Text ]
  , publicPrefixes :: [ Text ]
  , maxAge :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)


defaultCorsPolicy = CorsConfig {
   allowedOrigins = [ "http://localhost", "http://localhost:7885" ]
  , publicPrefixes = [ "inlogin", "site", "/" ]
  , maxAge = Nothing
  }


setCorsPolicy :: CorsConfig -> Middleware
setCorsPolicy CorsConfig{..} = cors $ \request ->
  if isSwaggerRequest request || isPublicApi request
  then Just $ simpleCorsResourcePolicy { 
    corsMethods = simpleMethods <> [ "OPTIONS" ]
    , corsRequestHeaders = simpleHeaders <> ["Authorization", "Content-Type"]
    }
  else Just $ CorsResourcePolicy { 
    corsOrigins = matchHostOrigin request
    , corsMethods = simpleMethods <> [ "DELETE", "OPTIONS" ]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = maxAge
    , corsVaryOrigin = True
    , corsRequireOrigin = True
    , corsIgnoreFailures = False
    }
    where
      matchHostOrigin request = Just . fromMaybe ([], False) $ do
        origin <- lookup hOrigin (requestHeaders request)

        if CI.mk origin `elem` (mk . encodeUtf8 <$> allowedOrigins)
        then return ([origin], True)
        else Nothing

      isSwaggerRequest request = case pathInfo request of
        []    -> False
        (x:_) -> "swagger" `DT.isPrefixOf` x

      isPublicApi request = case pathInfo request of
        []    -> False
        (x:_) -> any (`DT.isPrefixOf` x) publicPrefixes
