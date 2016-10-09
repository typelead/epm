module Distribution.Client.Patch where

import Distribution.Package        ( PackageKey )
import System.FilePath ( FilePath )
import qualified Data.ByteString.Lazy as BS

patchedTarPackageCabalFile :: FilePath -> IO (Maybe (FilePath, BS.ByteString))
patchedPackageCabalFile :: PackageKey -> IO (Maybe BS.ByteString)