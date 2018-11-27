{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_phone_number (
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
version = Version [1,6,1,8] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\phone-number\\.stack-work\\install\\dbe016db\\bin"
libdir     = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\phone-number\\.stack-work\\install\\dbe016db\\lib\\x86_64-windows-ghc-8.4.3\\phone-number-1.6.1.8-HUzWdMt0hK25iXM4tfNyss-test"
dynlibdir  = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\phone-number\\.stack-work\\install\\dbe016db\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\phone-number\\.stack-work\\install\\dbe016db\\share\\x86_64-windows-ghc-8.4.3\\phone-number-1.6.1.8"
libexecdir = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\phone-number\\.stack-work\\install\\dbe016db\\libexec\\x86_64-windows-ghc-8.4.3\\phone-number-1.6.1.8"
sysconfdir = "C:\\Users\\chen.ganshen\\Exercism\\haskell\\phone-number\\.stack-work\\install\\dbe016db\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "phone_number_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "phone_number_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "phone_number_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "phone_number_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "phone_number_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "phone_number_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
