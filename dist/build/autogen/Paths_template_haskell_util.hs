module Paths_template_haskell_util (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\template-haskell-util-0.1.0.0"
datadir    = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\template-haskell-util-0.1.0.0"
libexecdir = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\template-haskell-util-0.1.0.0"
sysconfdir = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "template_haskell_util_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "template_haskell_util_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "template_haskell_util_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "template_haskell_util_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "template_haskell_util_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
