{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

-- | An experimental new UI for cabal for working with multiple packages
-----------------------------------------------------------------------------
module Distribution.Client.ProjectPlanOutput (
    writePlanExternalRepresentation,
    writePlanGhcEnvironment,
  ) where

import           Distribution.Client.ProjectPlanning.Types
import           Distribution.Client.DistDirLayout
import           Distribution.Client.Types

import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.Utils.Json as J

import qualified Distribution.Solver.Types.ComponentDeps as ComponentDeps

import           Distribution.Package
import qualified Distribution.PackageDescription as PD
import           Distribution.Compiler (CompilerFlavor(GHC))
import           Distribution.Simple.Compiler
                   ( PackageDB(..), compilerVersion, compilerFlavor )
import           Distribution.Simple.GHC
                   ( getImplInfo, GhcImplInfo(supportsPkgEnvFiles)
                   , GhcEnvironmentFileEntry(..), simpleGhcEnvironmentFile
                   , writeGhcEnvironmentFile )
import           Distribution.Text
import           Distribution.Simple.Utils
import qualified Paths_cabal_install as Our (version)

import           Data.Monoid
import qualified Data.ByteString.Builder as BB


-- | Write out a representation of the elaborated install plan.
--
-- This is for the benefit of debugging and external tools like editors.
--
writePlanExternalRepresentation :: DistDirLayout
                                -> ElaboratedInstallPlan
                                -> ElaboratedSharedConfig
                                -> IO ()
writePlanExternalRepresentation distDirLayout elaboratedInstallPlan
                                elaboratedSharedConfig =
    writeFileAtomic (distProjectCacheFile distDirLayout "plan.json") $
        BB.toLazyByteString
      . J.encodeToBuilder
      $ encodePlanAsJson elaboratedInstallPlan elaboratedSharedConfig

-- | Renders a subset of the elaborated install plan in a semi-stable JSON
-- format.
--
encodePlanAsJson :: ElaboratedInstallPlan -> ElaboratedSharedConfig -> J.Value
encodePlanAsJson elaboratedInstallPlan _elaboratedSharedConfig =
    --TODO: [nice to have] include all of the sharedPackageConfig and all of
    --      the parts of the elaboratedInstallPlan
    J.object [ "cabal-version"     J..= jdisplay Our.version
             , "cabal-lib-version" J..= jdisplay cabalVersion
             , "install-plan"      J..= jsonIPlan
             ]
  where
    jsonIPlan = map toJ (InstallPlan.toList elaboratedInstallPlan)

    -- ipi :: InstalledPackageInfo
    toJ (InstallPlan.PreExisting ipi) =
      -- installed packages currently lack configuration information
      -- such as their flag settings or non-lib components.
      --
      -- TODO: how to find out whether package is "local"?
      J.object
        [ "type"       J..= J.String "pre-existing"
        , "id"         J..= jdisplay (installedUnitId ipi)
        , "depends" J..= map jdisplay (installedDepends ipi)
        ]

    -- pkg :: ElaboratedPackage
    toJ (InstallPlan.Configured elab) =
      J.object $
        [ "type"       J..= J.String "configured"
        , "id"         J..= (jdisplay . installedUnitId) elab
        , "flags"      J..= J.object [ fn J..= v
                                     | (PD.FlagName fn,v) <-
                                            elabFlagAssignment elab ]
        ] ++
        case elabPkgOrComp elab of
          ElabPackage pkg ->
            let components = J.object $
                  [ comp2str c J..= J.object
                    [ "depends"     J..= map (jdisplay . confInstId) ldeps
                    , "exe-depends" J..= map (jdisplay . confInstId) edeps ]
                  | (c,(ldeps,edeps))
                      <- ComponentDeps.toList $
                         ComponentDeps.zip (pkgLibDependencies pkg)
                                           (pkgExeDependencies pkg) ]
            in ["components" J..= components]
          ElabComponent _ ->
            ["depends"     J..= map (jdisplay . confInstId) (elabLibDependencies elab)
            ,"exe-depends" J..= map jdisplay (elabExeDependencies elab)]

    -- TODO: maybe move this helper to "ComponentDeps" module?
    --       Or maybe define a 'Text' instance?
    comp2str :: ComponentDeps.Component -> String
    comp2str c = case c of
        ComponentDeps.ComponentLib     -> "lib"
        ComponentDeps.ComponentSubLib s -> "lib:"   <> s
        ComponentDeps.ComponentExe s   -> "exe:"   <> s
        ComponentDeps.ComponentTest s  -> "test:"  <> s
        ComponentDeps.ComponentBench s -> "bench:" <> s
        ComponentDeps.ComponentSetup   -> "setup"

    jdisplay :: Text a => a -> J.Value
    jdisplay = J.String . display


writePlanGhcEnvironment :: FilePath
                        -> ElaboratedInstallPlan
                        -> ElaboratedSharedConfig
                        -> IO ()
writePlanGhcEnvironment projectRootDir
                        elaboratedInstallPlan
                        ElaboratedSharedConfig {
                          pkgConfigCompiler = compiler,
                          pkgConfigPlatform = platform
                        }
  | compilerFlavor compiler == GHC
  , supportsPkgEnvFiles (getImplInfo compiler)
  --TODO: check ghcjs compat
  = writeGhcEnvironmentFile
      projectRootDir
      platform (compilerVersion compiler)
      (renderGhcEnviromentFile elaboratedInstallPlan)

writePlanGhcEnvironment _ _ _ = return ()

renderGhcEnviromentFile :: ElaboratedInstallPlan
                        -> [GhcEnvironmentFileEntry]
renderGhcEnviromentFile elaboratedInstallPlan =
    --TODO: extend the syntax so we can include comments like this
    -- map GhcEnvFileComment _headerComment ++
    simpleGhcEnvironmentFile packageDBs unitIds
  where
    _headerComment =
      [ "This is a GHC environment file written by cabal. This means you can"
      , "run ghc or ghci and get the environment of the project as a whole."
      , "But you still need to use cabal repl $target to get the environment"
      , "of specific components (libs, exes, tests etc) because each one can"
      , "have its own source dirs, cpp flags etc."
      , "" ]
    packageDBs = [GlobalPackageDB] --TODO: get the right ones
    unitIds    =
      [ installedUnitId pkg
      | pkg <- InstallPlan.toList elaboratedInstallPlan ]
      --TODO: need PreExisting and Installed state pkgs but not Configured
      -- these are the ones that are up to date.

