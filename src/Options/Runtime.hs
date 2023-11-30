module Options.Runtime (defaultRun, RunOptions (..), WebServerOptions (..)) where

import Data.Text (Text)

import WebServer.CorsPolicy (CorsConfig, defaultCorsPolicy)

data WebServerOptions = WebServerOptions {
    port :: Int
    , host :: Text
  }
  deriving (Show)

data RunOptions = RunOptions {
    debug :: Int
    , webServer :: WebServerOptions
    , jwkConfFile :: Maybe FilePath
    , corsPolicy :: Maybe CorsConfig
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
    , webServer = WebServerOptions {
        port = 8181
        , host = "localhost"
      }
    , jwkConfFile = Nothing
    , corsPolicy = Nothing
  }
