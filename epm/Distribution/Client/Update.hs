-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Update
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Update
    ( update
    ) where

import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..), LocalRepo(..) )
import Distribution.Client.HttpUtils
         ( DownloadResult(..) )
import Distribution.Client.FetchUtils
         ( downloadIndex )
import Distribution.Client.IndexUtils
         ( updateRepoIndexCache )
import Distribution.Client.Config
         ( defaultPatchesDir )
import Distribution.Client.Config
         ( defaultPatchesDir )

import Distribution.Simple.Configure
         ( etaHackageUrl )
import Distribution.Simple.Program
         ( gitProgram, defaultProgramConfiguration, runProgramInvocation, programInvocation,
           requireProgramVersion )
import Distribution.Simple.Utils
         ( writeFileAtomic, warn, notice )
import Distribution.Version
         ( Version(..), orLaterVersion )
import Distribution.Verbosity
         ( Verbosity )

import Distribution.Client.GZipUtils ( maybeDecompress )
import System.FilePath               ( dropExtension )
import System.Directory              ( doesDirectoryExist )
import qualified Data.ByteString.Lazy       as BS

-- | 'update' downloads the package list from all known servers
update :: Verbosity -> [Repo] -> IO ()
update verbosity [] =
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
update verbosity repos = do
  mapM_ (updateRepo verbosity) repos
  updatePatchRepo verbosity

updateRepo :: Verbosity -> Repo -> IO ()
updateRepo verbosity repo = case repoKind repo of
  Right LocalRepo -> return ()
  Left remoteRepo -> do
    notice verbosity $ "Downloading the latest package list from "
                    ++ remoteRepoName remoteRepo
    downloadResult <- downloadIndex verbosity (repoIndexType repo)
                                    remoteRepo (repoLocalDir repo)
    case downloadResult of
      FileAlreadyInCache -> return ()
      FileDownloaded indexPath -> do
        writeFileAtomic (dropExtension indexPath) . maybeDecompress
                                                =<< BS.readFile indexPath
        updateRepoIndexCache verbosity repo


-- git only supports the -C flag as of 1.8.5
-- See  http://stackoverflow.com/questions/5083224/git-pull-while-not-in-a-git-directory
updatePatchRepo :: Verbosity -> IO ()
updatePatchRepo verbosity = do
  notice verbosity $ "Updating the eta-hackage patch set"
  (gitProg, _, _) <- requireProgramVersion verbosity
                     gitProgram
                     (orLaterVersion (Version [1,8,5] []))
                     defaultProgramConfiguration
  let runGit = runProgramInvocation verbosity . programInvocation gitProg
  patchesDir <- defaultPatchesDir
  exists <- doesDirectoryExist patchesDir
  if exists
  then runGit ["-C", patchesDir, "pull"]
  else
     runGit ["clone", "--depth=1", "--config", "core.autocrlf=false", etaHackageUrl, patchesDir]

