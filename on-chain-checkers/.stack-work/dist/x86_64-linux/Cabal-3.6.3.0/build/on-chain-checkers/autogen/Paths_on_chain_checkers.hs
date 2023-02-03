{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_on_chain_checkers (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/max/Documents/university/third_year/dissertation/checkers/on-chain-checkers/.stack-work/install/x86_64-linux/cc87d641ef8d0fd0a84f0d78498de2c29655f613b52f54259eaf42e88a29efed/9.2.5/bin"
libdir     = "/home/max/Documents/university/third_year/dissertation/checkers/on-chain-checkers/.stack-work/install/x86_64-linux/cc87d641ef8d0fd0a84f0d78498de2c29655f613b52f54259eaf42e88a29efed/9.2.5/lib/x86_64-linux-ghc-9.2.5/on-chain-checkers-0.1.0.0-EYe5Ab9tdup7CIpkQjgoTs-on-chain-checkers"
dynlibdir  = "/home/max/Documents/university/third_year/dissertation/checkers/on-chain-checkers/.stack-work/install/x86_64-linux/cc87d641ef8d0fd0a84f0d78498de2c29655f613b52f54259eaf42e88a29efed/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/max/Documents/university/third_year/dissertation/checkers/on-chain-checkers/.stack-work/install/x86_64-linux/cc87d641ef8d0fd0a84f0d78498de2c29655f613b52f54259eaf42e88a29efed/9.2.5/share/x86_64-linux-ghc-9.2.5/on-chain-checkers-0.1.0.0"
libexecdir = "/home/max/Documents/university/third_year/dissertation/checkers/on-chain-checkers/.stack-work/install/x86_64-linux/cc87d641ef8d0fd0a84f0d78498de2c29655f613b52f54259eaf42e88a29efed/9.2.5/libexec/x86_64-linux-ghc-9.2.5/on-chain-checkers-0.1.0.0"
sysconfdir = "/home/max/Documents/university/third_year/dissertation/checkers/on-chain-checkers/.stack-work/install/x86_64-linux/cc87d641ef8d0fd0a84f0d78498de2c29655f613b52f54259eaf42e88a29efed/9.2.5/etc"

getBinDir     = catchIO (getEnv "on_chain_checkers_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "on_chain_checkers_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "on_chain_checkers_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "on_chain_checkers_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "on_chain_checkers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "on_chain_checkers_sysconfdir") (\_ -> return sysconfdir)




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
