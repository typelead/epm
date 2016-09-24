module Distribution.Client.Patch where

import System.FilePath ( FilePath )
import qualified Data.ByteString.Lazy as BS

patchedPackageCabalFile :: FilePath -> IO (Maybe (FilePath, BS.ByteString))