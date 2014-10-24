module Paths_rme (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/rjayatilleka/.cabal/bin"
libdir     = "/Users/rjayatilleka/.cabal/lib/rme-0.1/ghc-7.6.3"
datadir    = "/Users/rjayatilleka/.cabal/share/rme-0.1"
libexecdir = "/Users/rjayatilleka/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "rme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rme_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "rme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rme_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
