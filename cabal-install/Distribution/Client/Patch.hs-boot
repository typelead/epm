module Distribution.Client.Patch where

import System.FilePath ( FilePath )
import qualified Data.ByteString.Lazy as BS

type TarFilePath = String

patchedPackageCabalFile :: TarFilePath -> IO (Maybe (FilePath, BS.ByteString))