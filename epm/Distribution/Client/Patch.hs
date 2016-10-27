module Distribution.Client.Patch
  ( patchedTarPackageCabalFile
  , patchedExtractTarGzFile
  , patchedPackageCabalFile
  )
where

import Distribution.Package        ( PackageIdentifier(..), PackageName(..))
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

patchedPackageCabalFile :: PackageIdentifier 
                        -> IO FilePath
                        -> IO (Maybe BS.ByteString)
patchedPackageCabalFile
  (PackageIdentifier
    { pkgName = name
    , pkgVersion = Version { versionBranch = versions } }) patchesDir
  = findCabalFilePatch (unPackageName name
                      ++ "-"
                      ++ (intercalate "." $ map show versions)
                      <.> "cabal") patchesDir

patchedTarPackageCabalFile :: FilePath 
                           -> IO FilePath
                           -> IO (Maybe (FilePath, BS.ByteString))
patchedTarPackageCabalFile tarFilePath patchesDir' = 
  fmap (fmap (\bs -> (cabalFile, bs))) $ findCabalFilePatch cabalFile patchesDir'
  where packageAndVersion = dropExtension . dropExtension $ tarFilePath
        cabalFile = packageAndVersion <.> "cabal"

findCabalFilePatch :: FilePath 
                   -> IO FilePath -- ^ Filepath of the patches directory
                   -> IO (Maybe BS.ByteString)
findCabalFilePatch cabalFile patchesDir' = do
  patchesDir <- patchesDir'
  -- TODO: Speed this up with a cache?
  let cabalPatchLocation = patchesDir </> "patches" </> cabalFile
  exists <- doesFileExist cabalPatchLocation
  if exists
  then fmap Just $ BS.readFile cabalPatchLocation
  else return Nothing

patchedExtractTarGzFile :: Verbosity 
                        -> FilePath -- ^ Destination directory of tar.gz file
                        -> FilePath -- ^ Expected subdir (to check for tarbombs)
                        -> FilePath -- ^ Tarball
                        -> IO FilePath -- ^ Filepath of the patches directory
                        -> IO ()
patchedExtractTarGzFile verbosity dir expected tar patchesDir' = do
  patchesDir <- patchesDir'
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
    notice verbosity $ "Found patch in eta-hackage for " ++ expected
    runGit ["-C", gitDir, "init"]
    runGit ["-C", gitDir, "apply",
            "--ignore-space-change", "--ignore-whitespace"
           , patchFileLocation]
  where packageAndVersion = takeFileName expected
        patchFile = packageAndVersion <.> "patch"
