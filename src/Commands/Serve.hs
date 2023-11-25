
module Commands.Serve where

import WebServer.Server (listen)

import qualified Options.Runtime as Rto

serveCmd :: Rto.RunOptions -> IO ()
serveCmd rtOpts = do
  rezA <- listen rtOpts
  pure ()
