module Distribution.Client.Patch where

import Distribution.Client.Config ( defaultPatchesDir )

import System.FilePath            ( FilePath, dropExtension, (</>), (<.>) )
import System.Directory           ( doesFileExist )

import qualified Data.ByteString.Lazy as BS

type TarFilePath = String

patchedPackageCabalFile :: TarFilePath -> IO (Maybe (FilePath, BS.ByteString))
patchedPackageCabalFile tarFilePath = do
  patchesDir <- defaultPatchesDir
  -- TODO: Speed this up with a cache?
  let cabalPatchLocation = patchesDir </> "patches" </> cabalFile
  exists <- doesFileExist cabalPatchLocation
  if exists
  then BS.readFile cabalPatchLocation >>= (\bs -> return $ Just (cabalFile, bs))
  else return Nothing
  where packageAndVersion = dropExtension . dropExtension $ tarFilePath
        cabalFile = packageAndVersion <.> "cabal"
