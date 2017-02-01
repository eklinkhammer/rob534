{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hw1_code (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/klinkhae/class/rob534/HW1/hw1-code/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/bin"
libdir     = "/Users/klinkhae/class/rob534/HW1/hw1-code/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/lib/x86_64-osx-ghc-8.0.1/hw1-code-0.1.0.0-8aYr2pHdeuo3H0LfJWJHED"
dynlibdir  = "/Users/klinkhae/class/rob534/HW1/hw1-code/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/lib/x86_64-osx-ghc-8.0.1"
datadir    = "/Users/klinkhae/class/rob534/HW1/hw1-code/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/share/x86_64-osx-ghc-8.0.1/hw1-code-0.1.0.0"
libexecdir = "/Users/klinkhae/class/rob534/HW1/hw1-code/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/libexec"
sysconfdir = "/Users/klinkhae/class/rob534/HW1/hw1-code/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw1_code_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw1_code_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw1_code_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw1_code_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw1_code_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw1_code_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
