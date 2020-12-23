{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Advent_of_Code (
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

bindir     = "D:\\Users\\Thomas\\Documents\\Advent-of-Code\\.stack-work\\install\\11fe4572\\bin"
libdir     = "D:\\Users\\Thomas\\Documents\\Advent-of-Code\\.stack-work\\install\\11fe4572\\lib\\x86_64-windows-ghc-8.6.5\\Advent-of-Code-0.1.0.0-3sc1HAuJCUN4yZwsW7AAos"
dynlibdir  = "D:\\Users\\Thomas\\Documents\\Advent-of-Code\\.stack-work\\install\\11fe4572\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "D:\\Users\\Thomas\\Documents\\Advent-of-Code\\.stack-work\\install\\11fe4572\\share\\x86_64-windows-ghc-8.6.5\\Advent-of-Code-0.1.0.0"
libexecdir = "D:\\Users\\Thomas\\Documents\\Advent-of-Code\\.stack-work\\install\\11fe4572\\libexec\\x86_64-windows-ghc-8.6.5\\Advent-of-Code-0.1.0.0"
sysconfdir = "D:\\Users\\Thomas\\Documents\\Advent-of-Code\\.stack-work\\install\\11fe4572\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Advent_of_Code_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Advent_of_Code_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Advent_of_Code_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Advent_of_Code_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Advent_of_Code_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Advent_of_Code_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
