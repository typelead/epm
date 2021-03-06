-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.FetchUtils
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for fetching packages
-----------------------------------------------------------------------------
module Distribution.Client.FetchUtils (

    -- * fetching packages
    fetchPackage,
    isFetched,
    checkFetched,

    -- ** specifically for repo packages
    fetchRepoTarball,
    fetchSourceRepo,

    -- * fetching other things
    downloadIndex,
  ) where

import Distribution.Client.Brancher
import Distribution.Client.Types
import Distribution.Client.HttpUtils
         ( downloadURI, isOldHackageURI, DownloadResult(..) )

import Distribution.Package
         ( PackageId, PackageName, packageName, packageVersion )
import Distribution.Simple.Utils
         ( notice, info, setupMessage )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Version        ( Version(..), orLaterVersion )
import Distribution.Simple.Program ( gitProgram, defaultProgramConfiguration
                                   , runProgramInvocation, programInvocation
                                   , getProgramInvocationOutput
                                   , requireProgramVersion )
import qualified Distribution.PackageDescription as PD

import Data.Maybe
import System.Directory
         ( doesFileExist, doesDirectoryExist, createDirectoryIfMissing,
           getTemporaryDirectory )
import System.IO
         ( openTempFile, hClose )
import System.FilePath
         ( (</>), (<.>) )
import qualified System.FilePath.Posix as FilePath.Posix
         ( combine, joinPath, splitDirectories )
import Network.URI
         ( URI(uriPath) )

-- ------------------------------------------------------------
-- * Actually fetch things
-- ------------------------------------------------------------

-- | Returns @True@ if the package has already been fetched
-- or does not need fetching.
--
isFetched :: PackageLocation (Maybe FilePath) -> IO Bool
isFetched loc = case loc of
    LocalUnpackedPackage _dir        -> return True
    LocalTarballPackage  _file       -> return True
    RemoteTarballPackage _uri local  -> return (isJust local)
    RepoTarballPackage repo pkgid _  -> doesFileExist (packageFile repo pkgid)
    ScmPackage (Just repo) _ pkgid _ ->
      doesDirectoryExist (packageDir repo pkgid)

checkFetched :: PackageLocation (Maybe FilePath)
             -> IO (Maybe (PackageLocation FilePath))
checkFetched loc = case loc of
    LocalUnpackedPackage dir  ->
      return (Just $ LocalUnpackedPackage dir)
    LocalTarballPackage  file ->
      return (Just $ LocalTarballPackage  file)
    RemoteTarballPackage uri (Just file) ->
      return (Just $ RemoteTarballPackage uri file)
    RepoTarballPackage repo pkgid (Just file) ->
      return (Just $ RepoTarballPackage repo pkgid file)
    ScmPackage maybeRepo srcRepos pkgid (Just file) ->
      return (Just $ ScmPackage maybeRepo srcRepos pkgid file)

    RemoteTarballPackage _uri Nothing -> return Nothing
    RepoTarballPackage repo pkgid Nothing -> do
      let file = packageFile repo pkgid
      exists <- doesFileExist file
      if exists
        then return (Just $ RepoTarballPackage repo pkgid file)
        else return Nothing
    ScmPackage (Just repo) srcRepos pkgid Nothing -> do
      let dir = packageDir repo pkgid
      exists <- doesDirectoryExist dir
      if exists
        then return (Just $ ScmPackage (Just repo) srcRepos pkgid dir)
        else return Nothing

-- | Fetch a package if we don't have it already.
--
fetchPackage :: Verbosity
             -> PackageLocation (Maybe FilePath)
             -> IO (PackageLocation FilePath)
fetchPackage verbosity loc = case loc of
    LocalUnpackedPackage dir  ->
      return (LocalUnpackedPackage dir)
    LocalTarballPackage  file ->
      return (LocalTarballPackage  file)
    RemoteTarballPackage uri (Just file) ->
      return (RemoteTarballPackage uri file)
    RepoTarballPackage repo pkgid (Just file) ->
      return (RepoTarballPackage repo pkgid file)
    ScmPackage maybeRepo sourceRepos pkgid (Just file) ->
      return (ScmPackage maybeRepo sourceRepos pkgid file)

    RemoteTarballPackage uri Nothing -> do
      path <- downloadTarballPackage uri
      return (RemoteTarballPackage uri path)
    RepoTarballPackage repo pkgid Nothing -> do
      local <- fetchRepoTarball verbosity repo pkgid
      return (RepoTarballPackage repo pkgid local)
    ScmPackage (Just repo) sourceRepos pkgid Nothing -> do
      local <- fetchSourceRepo verbosity repo pkgid sourceRepos
      return (ScmPackage (Just repo) sourceRepos pkgid local)
  where
    downloadTarballPackage uri = do
      notice verbosity ("Downloading " ++ show uri)
      tmpdir <- getTemporaryDirectory
      (path, hnd) <- openTempFile tmpdir "cabal-.tar.gz"
      hClose hnd
      _ <- downloadURI verbosity uri path
      return path


-- | Fetch a repo package if we don't have it already.
--
fetchRepoTarball :: Verbosity -> Repo -> PackageId -> IO FilePath
fetchRepoTarball verbosity repo pkgid = do
  fetched <- doesFileExist (packageFile repo pkgid)
  if fetched
    then do info verbosity $ display pkgid ++ " has already been downloaded."
            return (packageFile repo pkgid)
    else do setupMessage verbosity "Downloading" pkgid
            downloadRepoPackage
  where
    downloadRepoPackage = case repoKind repo of
      Right LocalRepo -> return (packageFile repo pkgid)

      Left remoteRepo -> do
        let uri  = packageURI remoteRepo pkgid
            dir  = packageDir       repo pkgid
            path = packageFile      repo pkgid
        createDirectoryIfMissing True dir
        _ <- downloadURI verbosity uri path
        return path

-- | Fetch a source control management repo package if we don't have it already.
--
fetchSourceRepo :: Verbosity -> Repo -> PackageId
                -> [PD.SourceRepo] -> IO FilePath
fetchSourceRepo verbosity repo pkgid sourceRepos = do
  fetched <- doesDirectoryExist repoDir
  if fetched
    then do info verbosity $ repoDir ++ " has already been downloaded."
            return repoDir
    else do notice verbosity $ "Downloading " ++ repoDir ++ "..."
            downloadSourceRepo
            return repoDir
  where
    repoDir = packageDir repo pkgid
    downloadSourceRepo = do
      branchers <- findUsableBranchers
      forkPackage verbosity branchers repoDir Nothing pkgid sourceRepos

-- | Downloads an index file to [config-dir/packages/serv-id].
--
downloadIndex :: Verbosity -> IndexType -> RemoteRepo -> FilePath
              -> IO DownloadResult
downloadIndex verbosity indexType repo cacheDir
  | indexType == GitIndex = do
      exists <- doesDirectoryExist cacheDir
      (gitProg, _, _) <- requireProgramVersion verbosity
                           gitProgram
                           (orLaterVersion (Version [1,8,5] []))
                           defaultProgramConfiguration
      let runGit = runProgramInvocation verbosity . programInvocation gitProg
      if exists
      then runGit ["-C", cacheDir, "pull"]
      else runGit ["clone", "--depth=1", show (remoteRepoURI repo), cacheDir]
      return FileAlreadyInCache

  | otherwise = do
      let uri = (remoteRepoURI repo) {
              uriPath = uriPath (remoteRepoURI repo)
                       `FilePath.Posix.combine` "00-index.tar.gz"
            }
          path = cacheDir </> "00-index" <.> "tar.gz"
      createDirectoryIfMissing True cacheDir
      downloadURI verbosity uri path

-- ------------------------------------------------------------
-- * Path utilities
-- ------------------------------------------------------------

-- | Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifer@.
--
packageFile :: Repo -> PackageId -> FilePath
packageFile repo pkgid = packageDir repo pkgid
                     </> display pkgid
                     <.> "tar.gz"

-- | Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifer@ is stored.
--
packageDir :: Repo -> PackageId -> FilePath
packageDir repo pkgid = repoLocalDir repo
                    </> display (packageName    pkgid)
                    </> display (packageVersion pkgid)

-- | Generate the URI of the tarball for a given package.
--
packageURI :: RemoteRepo -> PackageId -> URI
packageURI repo pkgid | isOldHackageURI (remoteRepoURI repo) =
  (remoteRepoURI repo) {
    uriPath = FilePath.Posix.joinPath
      [uriPath (remoteRepoURI repo)
      ,display (packageName    pkgid)
      ,display (packageVersion pkgid)
      ,display pkgid <.> "tar.gz"]
  }
packageURI repo pkgid =
  (remoteRepoURI repo) {
    uriPath = FilePath.Posix.joinPath
      [uriPath (remoteRepoURI repo)
      ,"package"
      ,display pkgid <.> "tar.gz"]
  }

repoCacheDir :: PD.SourceRepo -> FilePath -> FilePath
repoCacheDir sourceRepo cacheDir
  | null parts = error "repoCacheDir: Invalid rep url."
  | otherwise = FilePath.Posix.joinPath (cacheDir : repoType' : parts)
  where repoType' = PD.repoTypeLowercase $ fromJust $ PD.repoType sourceRepo
        parts     = drop 2
                  . FilePath.Posix.splitDirectories
                  . fromJust
                  $ PD.repoLocation sourceRepo
