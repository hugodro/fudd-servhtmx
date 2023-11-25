{-# LANGUAGE TemplateHaskell #-}

module Commands.Version where

import qualified Options.Runtime as Rto

import Data.Version (showVersion)
import Development.GitRev (gitHash, gitCommitDate)
import Paths_servhtmx (version)

versionCmd :: Rto.RunOptions -> IO ()
versionCmd _ = do
  putStrLn $ "Version: " <> showVersion version <> ", git " <> $(gitHash) <> " (" <> $(gitCommitDate) <> ")."
