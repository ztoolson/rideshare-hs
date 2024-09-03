{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_rideshare_hs (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/zachtoolson/workspace/rideshare-hs/.stack-work/install/x86_64-osx/56d9f564a8a11aaccd1249048f17e58f7d40f09567fe7da28fd6c3f731028d3b/9.2.8/bin"
libdir     = "/Users/zachtoolson/workspace/rideshare-hs/.stack-work/install/x86_64-osx/56d9f564a8a11aaccd1249048f17e58f7d40f09567fe7da28fd6c3f731028d3b/9.2.8/lib/x86_64-osx-ghc-9.2.8/rideshare-hs-0.0.0-4330D3hxcMKAQ2O9wwNHpL-rideshare-hs"
dynlibdir  = "/Users/zachtoolson/workspace/rideshare-hs/.stack-work/install/x86_64-osx/56d9f564a8a11aaccd1249048f17e58f7d40f09567fe7da28fd6c3f731028d3b/9.2.8/lib/x86_64-osx-ghc-9.2.8"
datadir    = "/Users/zachtoolson/workspace/rideshare-hs/.stack-work/install/x86_64-osx/56d9f564a8a11aaccd1249048f17e58f7d40f09567fe7da28fd6c3f731028d3b/9.2.8/share/x86_64-osx-ghc-9.2.8/rideshare-hs-0.0.0"
libexecdir = "/Users/zachtoolson/workspace/rideshare-hs/.stack-work/install/x86_64-osx/56d9f564a8a11aaccd1249048f17e58f7d40f09567fe7da28fd6c3f731028d3b/9.2.8/libexec/x86_64-osx-ghc-9.2.8/rideshare-hs-0.0.0"
sysconfdir = "/Users/zachtoolson/workspace/rideshare-hs/.stack-work/install/x86_64-osx/56d9f564a8a11aaccd1249048f17e58f7d40f09567fe7da28fd6c3f731028d3b/9.2.8/etc"

getBinDir     = catchIO (getEnv "rideshare_hs_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "rideshare_hs_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "rideshare_hs_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "rideshare_hs_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rideshare_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rideshare_hs_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
