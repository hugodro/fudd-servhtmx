{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_servant_htmx (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/x86_64-linux-ghc-8.6.5/servant-htmx-0.1.0.1-inplace"
dynlibdir  = "/root/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/root/.cabal/share/x86_64-linux-ghc-8.6.5/servant-htmx-0.1.0.1"
libexecdir = "/root/.cabal/libexec/x86_64-linux-ghc-8.6.5/servant-htmx-0.1.0.1"
sysconfdir = "/root/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "servant_htmx_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "servant_htmx_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "servant_htmx_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "servant_htmx_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "servant_htmx_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "servant_htmx_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
