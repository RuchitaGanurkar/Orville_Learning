{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_OrvilleProject (
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
bindir     = "/app/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/277f5894aa83dfae7035983f748fb4faf9a09afedffe0246fbf6e1f8a4c4fb87/9.6.6/bin"
libdir     = "/app/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/277f5894aa83dfae7035983f748fb4faf9a09afedffe0246fbf6e1f8a4c4fb87/9.6.6/lib/x86_64-linux-ghc-9.6.6/OrvilleProject-0.1.0.0-6RBWpzYcxkRJLXeZ35eXd4-OrvilleProject-exe"
dynlibdir  = "/app/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/277f5894aa83dfae7035983f748fb4faf9a09afedffe0246fbf6e1f8a4c4fb87/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/app/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/277f5894aa83dfae7035983f748fb4faf9a09afedffe0246fbf6e1f8a4c4fb87/9.6.6/share/x86_64-linux-ghc-9.6.6/OrvilleProject-0.1.0.0"
libexecdir = "/app/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/277f5894aa83dfae7035983f748fb4faf9a09afedffe0246fbf6e1f8a4c4fb87/9.6.6/libexec/x86_64-linux-ghc-9.6.6/OrvilleProject-0.1.0.0"
sysconfdir = "/app/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/277f5894aa83dfae7035983f748fb4faf9a09afedffe0246fbf6e1f8a4c4fb87/9.6.6/etc"

getBinDir     = catchIO (getEnv "OrvilleProject_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "OrvilleProject_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "OrvilleProject_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "OrvilleProject_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OrvilleProject_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "OrvilleProject_sysconfdir") (\_ -> return sysconfdir)



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
