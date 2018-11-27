{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_acronym (
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
version = Version [1,6,0,9] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\acronym\\.stack-work\\install\\dbe016db\\bin"
libdir     = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\acronym\\.stack-work\\install\\dbe016db\\lib\\x86_64-windows-ghc-8.4.3\\acronym-1.6.0.9-CpJUKN926uWB2574cnRNLv-test"
dynlibdir  = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\acronym\\.stack-work\\install\\dbe016db\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\acronym\\.stack-work\\install\\dbe016db\\share\\x86_64-windows-ghc-8.4.3\\acronym-1.6.0.9"
libexecdir = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\acronym\\.stack-work\\install\\dbe016db\\libexec\\x86_64-windows-ghc-8.4.3\\acronym-1.6.0.9"
sysconfdir = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\acronym\\.stack-work\\install\\dbe016db\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "acronym_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "acronym_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "acronym_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "acronym_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "acronym_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "acronym_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
