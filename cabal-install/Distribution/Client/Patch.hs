module Distribution.Client.Patch
  ( patchedTarPackageCabalFile
  , patchedExtractTarGzFile
  , patchedPackageCabalFile
  )
where

import Distribution.Package        ( PackageKey(..), PackageIdentifier(..), PackageName(..))
import Distribution.Simple.Program ( gitProgram, defaultProgramConfiguration
                                   , runProgramInvocation, programInvocation
                                   , requireProgramVersion )
import Distribution.Simple.Utils   ( notice )
import Distribution.Version        ( Version(..), orLaterVersion )
import Distribution.Verbosity      ( Verbosity )

import Distribution.Client.Tar     ( extractTarGzFile )
import Distribution.Client.Config  ( defaultPatchesDir )

import Data.Version                ( Version(..) )
import Data.List                   ( intercalate )
import Control.Monad               ( when )
import System.FilePath             ( FilePath, dropExtension, (</>), (<.>), takeFileName )
import System.Directory            ( doesFileExist )

import qualified Data.ByteString.Lazy as BS

patchedPackageCabalFile :: PackageKey -> IO (Maybe BS.ByteString)
patchedPackageCabalFile
  (OldPackageKey
   (PackageIdentifier
    { pkgName = name
    , pkgVersion = Version { versionBranch = versions } }))
  = findCabalFilePatch $ unPackageName name
                      ++ "-"
                      ++ (intercalate "." $ map show versions)
                      <.> "cabal"

patchedTarPackageCabalFile :: FilePath -> IO (Maybe (FilePath, BS.ByteString))
patchedTarPackageCabalFile tarFilePath =
  fmap (fmap (\bs -> (cabalFile, bs))) $ findCabalFilePatch cabalFile
  where packageAndVersion = dropExtension . dropExtension $ tarFilePath
        cabalFile = packageAndVersion <.> "cabal"

findCabalFilePatch :: FilePath -> IO (Maybe BS.ByteString)
findCabalFilePatch cabalFile = do
  patchesDir <- defaultPatchesDir
  -- TODO: Speed this up with a cache?
  let cabalPatchLocation = patchesDir </> "patches" </> cabalFile
  exists <- doesFileExist cabalPatchLocation
  if exists
  then fmap Just $ BS.readFile cabalPatchLocation
  else return Nothing

patchedExtractTarGzFile :: Verbosity -> FilePath -> FilePath -> FilePath -> IO ()
patchedExtractTarGzFile verbosity dir expected tar = do
  patchesDir <- defaultPatchesDir
  -- TODO: Speed this up with a cache?
  let patchFileLocation = patchesDir </> "patches" </> patchFile
  exists <- doesFileExist patchFileLocation
  extractTarGzFile dir expected tar
  when exists $ do
    (gitProg, _, _) <- requireProgramVersion verbosity
                      gitProgram
                      (orLaterVersion (Version [1,8,5] []))
                      defaultProgramConfiguration
    let runGit = runProgramInvocation verbosity . programInvocation gitProg
    let gitDir = dir </> expected
    notice verbosity $ "Found patch in ghcvm-hackage for " ++ expected
    runGit ["-C", gitDir, "init"]
    runGit ["-C", gitDir, "apply",
            "--ignore-space-change", "--ignore-whitespace"
           , patchFileLocation]
  where packageAndVersion = takeFileName expected
        patchFile = packageAndVersion <.> "patch"
