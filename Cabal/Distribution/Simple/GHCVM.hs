{-# LANGUAGE CPP #-}

module Distribution.Simple.GHCVM (
        configure, getInstalledPackages, getPackageDBContents,
        buildLib, buildExe,
        replLib, replExe,
        startInterpreter,
        installLib, installExe,
        libAbiHash,
        hcPkgInfo,
        registerPackage,
        componentGhcOptions,
        getLibDir,
        isDynamic,
        getGlobalPackageDB,
        runCmd
  ) where

import Distribution.Simple.GHC.ImplInfo ( getImplInfo, ghcvmVersionImplInfo )
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), Executable(..)
         , Library(..), libModules, exeModules
         , hcOptions, hcProfOptions, hcSharedOptions
         , allExtensions )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
                                ( InstalledPackageInfo_(..) )
import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..)
         , LibraryName(..) )
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.InstallDirs hiding ( absoluteInstallDirs )
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration
         , ProgramSearchPath
         , rawSystemProgramConf
         , rawSystemProgramStdout, rawSystemProgramStdoutConf
         , getProgramInvocationOutput
         , requireProgramVersion, requireProgram
         , userMaybeSpecifyPath, programPath
         , lookupProgram, addKnownPrograms
         , ghcvmProgram, ghcvmPkgProgram, c2hsProgram, hsc2hsProgram
         , ldProgram, haddockProgram, stripProgram )
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import qualified Distribution.Simple.Program.Ar    as Ar
import qualified Distribution.Simple.Program.Ld    as Ld
import qualified Distribution.Simple.Program.Strip as Strip
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup
         ( toFlag, fromFlag, configCoverage, configDistPref )
import qualified Distribution.Simple.Setup as Cabal
        ( Flag(..) )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..)
         , PackageDB(..), PackageDBStack, AbiTag(..) )
import Distribution.Version
         ( Version(..), anyVersion, orLaterVersion )
import Distribution.System
         ( Platform(..) )
import Distribution.Verbosity
import Distribution.Utils.NubList
         ( overNubListR, toNubListR )
import Distribution.Text ( display )
import Language.Haskell.Extension ( Extension(..)
                                  , KnownExtension(..))

import Control.Monad            ( unless, when )
import Data.Char                ( isSpace )
import Data.Version             ( makeVersion )
import qualified Data.Map as M  ( fromList  )
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid              ( Monoid(..) )
#endif
import System.Directory         ( doesFileExist )
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension,
                                  splitExtension )

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration
          -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf0 = do
  (ghcvmProg, ghcvmVersion, conf1) <-
    requireProgramVersion verbosity ghcvmProgram
      anyVersion --(orLaterVersion (Version [0,1] []))
      (userMaybeSpecifyPath "ghcvm" hcPath conf0)

  let implInfo = ghcvmVersionImplInfo ghcvmVersion ghcvmGhcVersion

  -- This is slightly tricky, we have to configure ghcvm first, then we use the
  -- location of ghcvm to help find ghcvm-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (ghcvmPkgProg, ghcvmPkgVersion, conf2) <-
    requireProgramVersion verbosity ghcvmPkgProgram
    {- TODO: Is this necessary? {programFindLocation = guessGhcvmPkgFromGhcvmPath ghcvmProg} -}
    anyVersion (userMaybeSpecifyPath "ghcvm-pkg" hcPkgPath conf1)

  -- Just ghcvmPkgGhcvmVersion <- findGhcvmPkgGhcvmVersion
  --                                 verbosity (programPath ghcvmPkgProg)

  when (ghcvmVersion /= ghcvmPkgVersion) $ die $
       "Version mismatch between ghcvm and ghcvm-pkg: "
    ++ programPath ghcvmProg ++ " is version " ++ display ghcvmVersion ++ " "
    ++ programPath ghcvmPkgProg ++ " is version " ++ display ghcvmPkgVersion

  -- when (ghcvmGhcVersion /= ghcvmPkgVersion) $ die $
  --      "Version mismatch between ghcvm and ghcvm-pkg: "
  --   ++ programPath ghcvmProg
  --   ++ " was built with GHC version " ++ display ghcvmGhcVersion ++ " "
  --   ++ programPath ghcvmPkgProg
  --   ++ " was built with GHC version " ++ display ghcvmPkgVersion

  -- be sure to use our versions of hsc2hs, c2hs, haddock and ghc
  -- let hsc2hsProgram' =
  --       hsc2hsProgram { programFindLocation =
  --                         guessHsc2hsFromGhcvmPath ghcvmProg }
  --     c2hsProgram' =
  --       c2hsProgram { programFindLocation =
  --                         guessC2hsFromGhcvmPath ghcvmProg }

  --     haddockProgram' =
  --       haddockProgram { programFindLocation =
  --                         guessHaddockFromGhcvmPath ghcvmProg }
  --     conf3 = addKnownPrograms [ hsc2hsProgram', c2hsProgram', haddockProgram' ] conf2
  let conf3 = conf2 -- TODO: Account for other programs

  languages  <- Internal.getLanguages  verbosity implInfo ghcvmProg
  extensions <- Internal.getExtensions verbosity implInfo ghcvmProg

  ghcvmInfo <- Internal.getGhcInfo verbosity implInfo ghcvmProg
  let ghcvmInfoMap = M.fromList ghcvmInfo

  let comp = Compiler {
        compilerId         = CompilerId GHCVM ghcvmVersion,
        -- TODO: Make this unique for GHCVM?
        compilerAbiTag     = AbiTag $
          "ghc" ++ intercalate "_" (map show . versionBranch $ ghcvmGhcVersion),
        compilerCompat     = [CompilerId GHC ghcvmGhcVersion],
        compilerLanguages  = languages,
        compilerExtensions = extensions,
        compilerProperties = ghcvmInfoMap
      }
      compPlatform = Nothing -- Internal.targetPlatform ghcInfo
  let conf4 = conf3
  return (comp, compPlatform, conf4)

ghcvmNativeToo :: Compiler -> Bool
ghcvmNativeToo = Internal.ghcLookupProperty "Native Too"

-- guessGhcvmPkgFromGhcvmPath :: ConfiguredProgram -> Verbosity
--                            -> ProgramSearchPath -> IO (Maybe FilePath)
-- guessGhcvmPkgFromGhcvmPath = guessToolFromGhcvmPath ghcvmPkgProgram

-- guessHsc2hsFromGhcvmPath :: ConfiguredProgram -> Verbosity
--                          -> ProgramSearchPath -> IO (Maybe FilePath)
-- guessHsc2hsFromGhcvmPath = guessToolFromGhcvmPath hsc2hsProgram

-- guessC2hsFromGhcvmPath :: ConfiguredProgram -> Verbosity
--                        -> ProgramSearchPath -> IO (Maybe FilePath)
-- guessC2hsFromGhcvmPath = guessToolFromGhcvmPath c2hsProgram

-- guessHaddockFromGhcvmPath :: ConfiguredProgram -> Verbosity
--                           -> ProgramSearchPath -> IO (Maybe FilePath)
-- guessHaddockFromGhcvmPath = guessToolFromGhcvmPath haddockProgram

-- guessToolFromGhcvmPath :: Program -> ConfiguredProgram
--                        -> Verbosity -> ProgramSearchPath
--                        -> IO (Maybe FilePath)
-- guessToolFromGhcvmPath tool ghcvmProg verbosity searchpath
--   = do let toolname          = programName tool
--            path              = programPath ghcvmProg
--            dir               = takeDirectory path
--            versionSuffix     = takeVersionSuffix (dropExeExtension path)
--            guessNormal       = dir </> toolname <.> exeExtension
--            guessGhcvmVersioned = dir </> (toolname ++ "-ghcvm" ++ versionSuffix)
--                                  <.> exeExtension
--            guessGhcvm        = dir </> (toolname ++ "-ghcvm")
--                                <.> exeExtension
--            guessVersioned    = dir </> (toolname ++ versionSuffix) <.> exeExtension
--            guesses | null versionSuffix = [guessGhcvm, guessNormal]
--                    | otherwise          = [guessGhcvmVersioned,
--                                            guessGhcvm,
--                                            guessVersioned,
--                                            guessNormal]
--        info verbosity $ "looking for tool " ++ toolname
--          ++ " near compiler in " ++ dir
--        exists <- mapM doesFileExist guesses
--        case [ file | (file, True) <- zip guesses exists ] of
--                    -- If we can't find it near ghc, fall back to the usual
--                    -- method.
--          []     -> programFindLocation tool verbosity searchpath
--          (fp:_) -> do info verbosity $ "found " ++ toolname ++ " in " ++ fp
--                       return (Just fp)

--   where takeVersionSuffix :: FilePath -> String
--         takeVersionSuffix = reverse . takeWhile (`elem ` "0123456789.-") .
--                             reverse

--         dropExeExtension :: FilePath -> FilePath
--         dropExeExtension filepath =
--           case splitExtension filepath of
--             (filepath', extension) | extension == exeExtension -> filepath'
--                                    | otherwise                 -> filepath


-- | Given a single package DB, return all installed packages.
getPackageDBContents :: Verbosity -> PackageDB -> ProgramConfiguration
                     -> IO InstalledPackageIndex
getPackageDBContents verbosity packagedb conf = do
  pkgss <- getInstalledPackages' verbosity [packagedb] conf
  toPackageIndex verbosity pkgss conf

-- | Given a package DB stack, return all installed packages.
getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity packagedbs conf = do
  checkPackageDbEnvVar
  checkPackageDbStack packagedbs
  pkgss <- getInstalledPackages' verbosity packagedbs conf
  index <- toPackageIndex verbosity pkgss conf
  return $! index

toPackageIndex :: Verbosity
               -> [(PackageDB, [InstalledPackageInfo])]
               -> ProgramConfiguration
               -> IO InstalledPackageIndex
toPackageIndex verbosity pkgss conf = do
  -- On Windows, various fields have $topdir/foo rather than full
  -- paths. We need to substitute the right value in so that when
  -- we, for example, call gcc, we have proper paths to give it.
  topDir <- getLibDir' verbosity ghcvmProg
  let indices = [ PackageIndex.fromList (map (Internal.substTopDir topDir) pkgs)
                | (_, pkgs) <- pkgss ]
  return $! (mconcat indices)

  where
    Just ghcvmProg = lookupProgram ghcvmProgram conf

checkPackageDbEnvVar :: IO ()
checkPackageDbEnvVar =
    Internal.checkPackageDbEnvVar "GHCVM" "GHCVM_PACKAGE_PATH"

checkPackageDbStack :: PackageDBStack -> IO ()
checkPackageDbStack (GlobalPackageDB:rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStack rest
  | GlobalPackageDB `notElem` rest =
  die $ "With current ghc versions the global package db is always used "
     ++ "and must be listed first. This ghc limitation may be lifted in "
     ++ "future, see http://hackage.haskell.org/trac/ghc/ticket/5977"
checkPackageDbStack _ =
  die $ "If the global package db is specified, it must be "
     ++ "specified first and cannot be specified multiple times"

getInstalledPackages' :: Verbosity -> [PackageDB] -> ProgramConfiguration
                      -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity packagedbs conf =
  sequence
    [ do pkgs <- HcPkg.dump (hcPkgInfo conf) verbosity packagedb
         return (packagedb, pkgs)
    | packagedb <- packagedbs ]

getLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
getLibDir verbosity lbi =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdoutConf verbosity ghcvmProgram
     (withPrograms lbi) ["--print-libdir"]

getLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
getLibDir' verbosity ghcvmProg =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdout verbosity ghcvmProg ["--print-libdir"]

-- | Return the 'FilePath' to the global GHC package database.
getGlobalPackageDB :: Verbosity -> ConfiguredProgram -> IO FilePath
getGlobalPackageDB verbosity ghcvmProg =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdout verbosity ghcvmProg ["--print-global-package-db"]

toJSLibName :: String -> String
toJSLibName lib
  | takeExtension lib `elem` [".dll",".dylib",".so"]
                              = replaceExtension lib "js_so"
  | takeExtension lib == ".a" = replaceExtension lib "js_a"
  | otherwise                 = lib <.> "js_a"

buildLib, replLib :: Verbosity -> Cabal.Flag (Maybe Int) -> PackageDescription
                  -> LocalBuildInfo -> Library -> ComponentLocalBuildInfo
                  -> IO ()
buildLib = buildOrReplLib False
replLib  = buildOrReplLib True

buildOrReplLib :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Library            -> ComponentLocalBuildInfo -> IO ()
buildOrReplLib forRepl verbosity numJobs _pkg_descr lbi lib clbi = do
  libName <- case componentLibraries clbi of
             [libName] -> return libName
             [] -> die "No library name found when building library"
             _  -> die "Multiple library names found when building library"
  let libTargetDir = buildDir lbi
      whenVanillaLib forceVanilla =
        when (not forRepl && (forceVanilla || withVanillaLib lbi))
      whenProfLib = when (not forRepl && withProfLib lbi)
      whenSharedLib forceShared =
        when (not forRepl &&  (forceShared || withSharedLib lbi))
      whenGHCiLib = when (not forRepl && withGHCiLib lbi && withVanillaLib lbi)
      ifReplLib = when forRepl
      comp = compiler lbi
      implInfo = getImplInfo comp
      hole_insts = map (\(k,(p,n)) -> (k,(InstalledPackageInfo.packageKey p,n)))
                       (instantiatedWith lbi)
      nativeToo = False -- TODO: Remove

  (ghcvmProg, _) <- requireProgram verbosity ghcvmProgram (withPrograms lbi)
  let runGhcvmProg        = runGHC verbosity ghcvmProg comp
      libBi               = libBuildInfo lib
      isGhcvmDynamic      = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      doingTH = EnableExtension TemplateHaskell `elem` allExtensions libBi
      forceVanillaLib = doingTH && not isGhcvmDynamic
      forceSharedLib  = doingTH &&     isGhcvmDynamic
      -- TH always needs default libs, even when building for profiling

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  -- TODO: Implement HPC in GHCVM
  let isCoverageEnabled = fromFlag $ configCoverage $ configFlags lbi
      -- Component name. Not 'libName' because that has the "HS" prefix
      -- that GHC gives Haskell libraries.
      cname = display $ PD.package $ localPkgDescr lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way cname
        | otherwise = mempty

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let cObjs       = map (`replaceExtension` objExtension) (cSources libBi)
      jsSrcs      = jsSources libBi
      baseOpts    = componentGhcOptions verbosity lbi libBi clbi libTargetDir
      linkJsLibOpts = mempty {
                        ghcOptExtra = toNubListR $
                          [ "-link-js-lib"     , (\(LibraryName l) -> l) libName
                          , "-js-lib-outputdir", libTargetDir ] ++
                          concatMap (\x -> ["-js-lib-src",x]) jsSrcs
                      }
      vanillaOptsNoJsLib = baseOpts `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptNumJobs      = numJobs,
                      ghcOptPackageKey   = toFlag (pkgKey lbi),
                      ghcOptSigOf        = hole_insts,
                      ghcOptInputModules = toNubListR $ libModules lib,
                      ghcOptHPCDir       = hpcdir Hpc.Vanilla
                    }
      vanillaOpts = vanillaOptsNoJsLib `mappend` linkJsLibOpts

      profOpts    = adjustExts "p_hi" "p_o" vanillaOpts `mappend` mempty {
                        ghcOptProfilingMode = toFlag True,
                        ghcOptExtra         = toNubListR $
                                              ghcvmProfOptions libBi,
                        ghcOptHPCDir        = hpcdir Hpc.Prof
                      }
      sharedOpts  = adjustExts "dyn_hi" "dyn_o" vanillaOpts `mappend` mempty {
                        ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                        ghcOptFPic        = toFlag True,
                        ghcOptExtra       = toNubListR $
                                            ghcvmSharedOptions libBi,
                        ghcOptHPCDir      = hpcdir Hpc.Dyn
                      }
      linkerOpts = mempty {
                      ghcOptLinkOptions    = toNubListR $ PD.ldOptions libBi,
                      ghcOptLinkLibs       = toNubListR $ extraLibs libBi,
                      ghcOptLinkLibPath    = toNubListR $ extraLibDirs libBi,
                      ghcOptLinkFrameworks = toNubListR $ PD.frameworks libBi,
                      ghcOptInputFiles     =
                        toNubListR $ [libTargetDir </> x | x <- cObjs] ++ jsSrcs
                   }
      replOpts    = vanillaOptsNoJsLib {
                      ghcOptExtra        = overNubListR
                                           Internal.filterGhciFlags
                                           (ghcOptExtra vanillaOpts),
                      ghcOptNumJobs      = mempty
                    }
                    `mappend` linkerOpts
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeInteractive,
                      ghcOptOptimisation = toFlag GhcNoOptimisation
                    }

      vanillaSharedOpts = vanillaOpts `mappend`
                            mempty {
                              ghcOptDynLinkMode  = toFlag GhcStaticAndDynamic,
                              ghcOptDynHiSuffix  = toFlag "dyn_hi",
                              ghcOptDynObjSuffix = toFlag "dyn_o",
                              ghcOptHPCDir       = hpcdir Hpc.Dyn
                            }

  unless (forRepl || (null (libModules lib) && null jsSrcs && null cObjs)) $
    do let vanilla = whenVanillaLib forceVanillaLib (runGhcvmProg vanillaOpts)
           shared  = whenSharedLib  forceSharedLib  (runGhcvmProg sharedOpts)
           useDynToo = dynamicTooSupported &&
                       (forceVanillaLib || withVanillaLib lbi) &&
                       (forceSharedLib  || withSharedLib  lbi) &&
                       null (ghcvmSharedOptions libBi)
       if useDynToo
          then do
              runGhcvmProg vanillaSharedOpts
              case (hpcdir Hpc.Dyn, hpcdir Hpc.Vanilla) of
                (Cabal.Flag dynDir, Cabal.Flag vanillaDir) -> do
                    -- When the vanilla and shared library builds are done
                    -- in one pass, only one set of HPC module interfaces
                    -- are generated. This set should suffice for both
                    -- static and dynamically linked executables. We copy
                    -- the modules interfaces so they are available under
                    -- both ways.
                    copyDirectoryRecursive verbosity dynDir vanillaDir
                _ -> return ()
          else if isGhcvmDynamic
            then do shared;  vanilla
            else do vanilla; shared
       whenProfLib (runGhcvmProg profOpts)

  -- build any C sources
  unless (null (cSources libBi) || not nativeToo) $ do
     info verbosity "Building C Sources..."
     sequence_
       [ do let vanillaCcOpts =
                  (Internal.componentCcGhcOptions verbosity implInfo
                     lbi libBi clbi libTargetDir filename)
                profCcOpts    = vanillaCcOpts `mappend` mempty {
                                  ghcOptProfilingMode = toFlag True,
                                  ghcOptObjSuffix     = toFlag "p_o"
                                }
                sharedCcOpts  = vanillaCcOpts `mappend` mempty {
                                  ghcOptFPic        = toFlag True,
                                  ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                                  ghcOptObjSuffix   = toFlag "dyn_o"
                                }
                odir          = fromFlag (ghcOptObjDir vanillaCcOpts)
            createDirectoryIfMissingVerbose verbosity True odir
            runGhcvmProg vanillaCcOpts
            whenSharedLib forceSharedLib (runGhcvmProg sharedCcOpts)
            whenProfLib (runGhcvmProg profCcOpts)
       | filename <- cSources libBi]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  unless (null (libModules lib)) $
     ifReplLib (runGhcvmProg replOpts)

  -- link:
  when (nativeToo && not forRepl) $ do
    info verbosity "Linking..."
    let cProfObjs   = map (`replaceExtension` ("p_" ++ objExtension))
                      (cSources libBi)
        cSharedObjs = map (`replaceExtension` ("dyn_" ++ objExtension))
                      (cSources libBi)
        cid = compilerId (compiler lbi)
        vanillaLibFilePath = libTargetDir </> mkLibName            libName
        profileLibFilePath = libTargetDir </> mkProfLibName        libName
        sharedLibFilePath  = libTargetDir </> mkSharedLibName cid  libName
        ghciLibFilePath    = libTargetDir </> Internal.mkGHCiLibName libName

    hObjs     <- Internal.getHaskellObjects implInfo lib lbi
                      libTargetDir objExtension True
    hProfObjs <-
      if (withProfLib lbi)
              then Internal.getHaskellObjects implInfo lib lbi
                      libTargetDir ("p_" ++ objExtension) True
              else return []
    hSharedObjs <-
      if (withSharedLib lbi)
              then Internal.getHaskellObjects implInfo lib lbi
                      libTargetDir ("dyn_" ++ objExtension) False
              else return []

    unless (null hObjs && null cObjs) $ do

      let staticObjectFiles =
                 hObjs
              ++ map (libTargetDir </>) cObjs
          profObjectFiles =
                 hProfObjs
              ++ map (libTargetDir </>) cProfObjs
          ghciObjFiles =
                 hObjs
              ++ map (libTargetDir </>) cObjs
          dynamicObjectFiles =
                 hSharedObjs
              ++ map (libTargetDir </>) cSharedObjs
          -- After the relocation lib is created we invoke ghc -shared
          -- with the dependencies spelled out as -package arguments
          -- and ghc invokes the linker with the proper library paths
          ghcSharedLinkArgs =
              mempty {
                ghcOptShared             = toFlag True,
                ghcOptDynLinkMode        = toFlag GhcDynamicOnly,
                ghcOptInputFiles         = toNubListR dynamicObjectFiles,
                ghcOptOutputFile         = toFlag sharedLibFilePath,
                ghcOptPackageKey         = toFlag (pkgKey lbi),
                ghcOptNoAutoLinkPackages = toFlag True,
                ghcOptPackageDBs         = withPackageDB lbi,
                ghcOptPackages           = toNubListR $
                                           Internal.mkGhcOptPackages clbi,
                ghcOptLinkLibs           = toNubListR $ extraLibs libBi,
                ghcOptLinkLibPath        = toNubListR $ extraLibDirs libBi
              }

      whenVanillaLib False $ do
        Ar.createArLibArchive verbosity lbi vanillaLibFilePath staticObjectFiles

      whenProfLib $ do
        Ar.createArLibArchive verbosity lbi profileLibFilePath profObjectFiles

      whenGHCiLib $ do
        (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
        Ld.combineObjectFiles verbosity ldProg
          ghciLibFilePath ghciObjFiles

      whenSharedLib False $
        runGhcvmProg ghcSharedLinkArgs

-- | Start a REPL without loading any source files.
startInterpreter :: Verbosity -> ProgramConfiguration -> Compiler
                 -> PackageDBStack -> IO ()
startInterpreter verbosity conf comp packageDBs = do
  let replOpts = mempty {
        ghcOptMode       = toFlag GhcModeInteractive,
        ghcOptPackageDBs = packageDBs
        }
  checkPackageDbStack packageDBs
  (ghcvmProg, _) <- requireProgram verbosity ghcvmProgram conf
  runGHC verbosity ghcvmProg comp replOpts

buildExe, replExe :: Verbosity          -> Cabal.Flag (Maybe Int)
                  -> PackageDescription -> LocalBuildInfo
                  -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe = buildOrReplExe False
replExe  = buildOrReplExe True

buildOrReplExe :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildOrReplExe forRepl verbosity numJobs _pkg_descr lbi
  exe@Executable { exeName = exeName', modulePath = modPath } clbi = do

  (ghcvmProg, _) <- requireProgram verbosity ghcvmProgram (withPrograms lbi)
  let comp         = compiler lbi
      implInfo     = getImplInfo comp
      runGhcvmProg = runGHC verbosity ghcvmProg comp
      exeBi        = buildInfo exe

  -- exeNameReal, the name that GHC really uses (with .exe on Windows)
  let exeNameReal = exeName' <.>
                    (if takeExtension exeName' /= ('.':exeExtension)
                       then exeExtension
                       else "")

  let targetDir = (buildDir lbi) </> exeName'
  let exeDir    = targetDir </> (exeName' ++ "-tmp")
  createDirectoryIfMissingVerbose verbosity True targetDir
  createDirectoryIfMissingVerbose verbosity True exeDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = fromFlag $ configCoverage $ configFlags lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way exeName'
        | otherwise = mempty

  -- build executables

  srcMainFile         <- findFile (exeDir : hsSourceDirs exeBi) modPath
  let isGhcvmDynamic      = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      buildRunner = case clbi of
                       ExeComponentLocalBuildInfo {} -> False
                       _                             -> True
      isHaskellMain = elem (takeExtension srcMainFile) [".hs", ".lhs"]
      jsSrcs        = jsSources exeBi
      cSrcs         = cSources exeBi ++ [srcMainFile | not isHaskellMain]
      cObjs         = map (`replaceExtension` objExtension) cSrcs
      nativeToo     = ghcvmNativeToo comp
      baseOpts   = (componentGhcOptions verbosity lbi exeBi clbi exeDir)
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptInputFiles   = toNubListR $
                        [ srcMainFile | isHaskellMain],
                      ghcOptInputModules = toNubListR $
                        [ m | not isHaskellMain, m <- exeModules exe],
                      ghcOptExtra =
                        if buildRunner then toNubListR ["-build-runner"]
                                       else mempty
                    }
      staticOpts = baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticOnly,
                      ghcOptHPCDir         = hpcdir Hpc.Vanilla
                   }
      profOpts   = adjustExts "p_hi" "p_o" baseOpts `mappend` mempty {
                      ghcOptProfilingMode  = toFlag True,
                      ghcOptExtra          = toNubListR $ ghcvmProfOptions exeBi,
                      ghcOptHPCDir         = hpcdir Hpc.Prof
                    }
      dynOpts    = adjustExts "dyn_hi" "dyn_o" baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcDynamicOnly,
                      ghcOptExtra          = toNubListR $
                                             ghcvmSharedOptions exeBi,
                      ghcOptHPCDir         = hpcdir Hpc.Dyn
                    }
      dynTooOpts = adjustExts "dyn_hi" "dyn_o" staticOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticAndDynamic,
                      ghcOptHPCDir         = hpcdir Hpc.Dyn
                    }
      linkerOpts = mempty {
                      ghcOptLinkOptions    = toNubListR $ PD.ldOptions exeBi,
                      ghcOptLinkLibs       = toNubListR $ extraLibs exeBi,
                      ghcOptLinkLibPath    = toNubListR $ extraLibDirs exeBi,
                      ghcOptLinkFrameworks = toNubListR $ PD.frameworks exeBi,
                      ghcOptInputFiles     = toNubListR $
                                             [exeDir </> x | x <- cObjs] ++ jsSrcs
                   }
      replOpts   = baseOpts {
                      ghcOptExtra          = overNubListR
                                             Internal.filterGhciFlags
                                             (ghcOptExtra baseOpts)
                   }
                   -- For a normal compile we do separate invocations of ghc for
                   -- compiling as for linking. But for repl we have to do just
                   -- the one invocation, so that one has to include all the
                   -- linker stuff too, like -l flags and any .o files from C
                   -- files etc.
                   `mappend` linkerOpts
                   `mappend` mempty {
                      ghcOptMode           = toFlag GhcModeInteractive,
                      ghcOptOptimisation   = toFlag GhcNoOptimisation
                   }
      commonOpts  | withProfExe lbi = profOpts
                  | withDynExe  lbi = dynOpts
                  | otherwise       = staticOpts
      compileOpts | useDynToo = dynTooOpts
                  | otherwise = commonOpts
      withStaticExe = (not $ withProfExe lbi) && (not $ withDynExe lbi)

      -- For building exe's that use TH with -prof or -dynamic we actually have
      -- to build twice, once without -prof/-dynamic and then again with
      -- -prof/-dynamic. This is because the code that TH needs to run at
      -- compile time needs to be the vanilla ABI so it can be loaded up and run
      -- by the compiler.
      -- With dynamic-by-default GHC the TH object files loaded at compile-time
      -- need to be .dyn_o instead of .o.
      doingTH = EnableExtension TemplateHaskell `elem` allExtensions exeBi
      -- Should we use -dynamic-too instead of compiling twice?
      useDynToo = dynamicTooSupported && isGhcvmDynamic
                  && doingTH && withStaticExe && null (ghcvmSharedOptions exeBi)
      compileTHOpts | isGhcvmDynamic = dynOpts
                    | otherwise      = staticOpts
      compileForTH
        | forRepl      = False
        | useDynToo    = False
        | isGhcvmDynamic = doingTH && (withProfExe lbi || withStaticExe)
        | otherwise      = doingTH && (withProfExe lbi || withDynExe lbi)

      linkOpts = commonOpts `mappend`
                 linkerOpts `mappend` mempty {
                      ghcOptLinkNoHsMain   = toFlag (not isHaskellMain)
                 }

  -- Build static/dynamic object files for TH, if needed.
  when compileForTH $
    runGhcvmProg compileTHOpts { ghcOptNoLink  = toFlag True
                               , ghcOptNumJobs = numJobs }

  unless forRepl $
    runGhcvmProg compileOpts { ghcOptNoLink  = toFlag True
                             , ghcOptNumJobs = numJobs }

  -- build any C sources
  unless (null cSrcs || not nativeToo) $ do
   info verbosity "Building C Sources..."
   sequence_
     [ do let opts = (Internal.componentCcGhcOptions verbosity implInfo lbi exeBi
                         clbi exeDir filename) `mappend` mempty {
                       ghcOptDynLinkMode   = toFlag (if withDynExe lbi
                                                       then GhcDynamicOnly
                                                       else GhcStaticOnly),
                       ghcOptProfilingMode = toFlag (withProfExe lbi)
                     }
              odir = fromFlag (ghcOptObjDir opts)
          createDirectoryIfMissingVerbose verbosity True odir
          runGhcvmProg opts
     | filename <- cSrcs ]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  when forRepl $ runGhcvmProg replOpts

  -- link:
  unless forRepl $ do
    info verbosity "Linking..."
    runGhcvmProg linkOpts { ghcOptOutputFile = toFlag (targetDir </> exeNameReal) }

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic libraries
              -> FilePath  -- ^Build location
              -> PackageDescription
              -> Library
              -> ComponentLocalBuildInfo
              -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir _pkg lib clbi = do
  whenVanilla $ copyModuleFiles "js_hi"
  whenProf    $ copyModuleFiles "js_p_hi"
  whenShared  $ copyModuleFiles "js_dyn_hi"

  whenVanilla $ mapM_ (installOrdinary builtDir targetDir . toJSLibName) vanillaLibNames
  whenProf    $ mapM_ (installOrdinary builtDir targetDir . toJSLibName) profileLibNames
  whenShared  $ mapM_ (installShared   builtDir dynlibTargetDir . toJSLibName) sharedLibNames

  when (ghcvmNativeToo $ compiler lbi) $ do
    -- copy .hi files over:
    whenVanilla $ copyModuleFiles "hi"
    whenProf    $ copyModuleFiles "p_hi"
    whenShared  $ copyModuleFiles "dyn_hi"

    -- copy the built library files over:
    whenVanilla $ mapM_ (installOrdinary builtDir targetDir)       vanillaLibNames
    whenProf    $ mapM_ (installOrdinary builtDir targetDir)       profileLibNames
    whenGHCi    $ mapM_ (installOrdinary builtDir targetDir)       ghciLibNames
    whenShared  $ mapM_ (installShared   builtDir dynlibTargetDir) sharedLibNames

  where
    install isShared srcDir dstDir name = do
      let src = srcDir </> name
          dst = dstDir </> name
      createDirectoryIfMissingVerbose verbosity True dstDir
      if isShared
        then do when (stripLibs lbi) $ Strip.stripLib verbosity
                                       (hostPlatform lbi) (withPrograms lbi) src
                installExecutableFile verbosity src dst
        else installOrdinaryFile   verbosity src dst

    installOrdinary = install False
    installShared   = install True

    copyModuleFiles ext =
      findModuleFiles [builtDir] [ext] (libModules lib)
      >>= installOrdinaryFiles verbosity targetDir

    cid = compilerId (compiler lbi)
    libNames = componentLibraries clbi
    vanillaLibNames = map mkLibName              libNames
    profileLibNames = map mkProfLibName          libNames
    ghciLibNames    = map Internal.mkGHCiLibName libNames
    sharedLibNames  = map (mkSharedLibName cid)  libNames

    hasLib    = not $ null (libModules lib)
                   && null (cSources (libBuildInfo lib))
    whenVanilla = when (hasLib && withVanillaLib lbi)
    whenProf    = when (hasLib && withProfLib    lbi)
    whenGHCi    = when (hasLib && withGHCiLib    lbi)
    whenShared  = when (hasLib && withSharedLib  lbi)

installExe :: Verbosity
              -> LocalBuildInfo
              -> InstallDirs FilePath -- ^Where to copy the files to
              -> FilePath  -- ^Build location
              -> (FilePath, FilePath)  -- ^Executable (prefix,suffix)
              -> PackageDescription
              -> Executable
              -> IO ()
installExe verbosity lbi installDirs buildPref
           (progprefix, progsuffix) _pkg exe = do
  let binDir = bindir installDirs
  createDirectoryIfMissingVerbose verbosity True binDir
  let exeFileName = exeName exe
      fixedExeBaseName = progprefix ++ exeName exe ++ progsuffix
      installBinary dest = do
        rawSystemProgramConf verbosity ghcvmProgram (withPrograms lbi) $
          [ "--install-executable"
          , buildPref </> exeName exe </> exeFileName
          , "-o", dest
          ] ++
          case (stripExes lbi, lookupProgram stripProgram $ withPrograms lbi) of
           (True, Just strip) -> ["-strip-program", programPath strip]
           _                  -> []
  installBinary (binDir </> fixedExeBaseName)

libAbiHash :: Verbosity -> PackageDescription -> LocalBuildInfo
           -> Library -> ComponentLocalBuildInfo -> IO String
libAbiHash verbosity _pkg_descr lbi lib clbi = do
  let
      libBi       = libBuildInfo lib
      comp        = compiler lbi
      vanillaArgs =
        (componentGhcOptions verbosity lbi libBi clbi (buildDir lbi))
        `mappend` mempty {
          ghcOptMode         = toFlag GhcModeAbiHash,
          ghcOptPackageKey   = toFlag (pkgKey lbi),
          ghcOptInputModules = toNubListR $ exposedModules lib
        }
      -- profArgs = adjustExts "js_p_hi" "js_p_o" vanillaArgs `mappend` mempty {
      --                ghcOptProfilingMode = toFlag True,
      --                ghcOptExtra         = toNubListR (ghcvmProfOptions libBi)
      --            }
      ghcArgs = if withVanillaLib lbi then vanillaArgs
--           else if withProfLib    lbi then profArgs
                else error "libAbiHash: Can't find an enabled library way"
  --
  (ghcvmProg, _) <- requireProgram verbosity ghcvmProgram (withPrograms lbi)
  getProgramInvocationOutput verbosity (ghcInvocation ghcvmProg comp ghcArgs)

adjustExts :: String -> String -> GhcOptions -> GhcOptions
adjustExts hiSuf objSuf opts =
  opts `mappend` mempty {
    ghcOptHiSuffix  = toFlag hiSuf,
    ghcOptObjSuffix = toFlag objSuf
  }

registerPackage :: Verbosity
                -> InstalledPackageInfo
                -> PackageDescription
                -> LocalBuildInfo
                -> Bool
                -> PackageDBStack
                -> IO ()
registerPackage verbosity installedPkgInfo _pkg lbi _inplace packageDbs =
  HcPkg.reregister (hcPkgInfo $ withPrograms lbi) verbosity packageDbs
    (Right installedPkgInfo)

componentGhcOptions :: Verbosity -> LocalBuildInfo
                    -> BuildInfo -> ComponentLocalBuildInfo -> FilePath
                    -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
  let opts = Internal.componentGhcOptions verbosity lbi bi clbi odir
  in  opts { ghcOptExtra = ghcOptExtra opts `mappend` toNubListR
                             (hcOptions GHCVM bi)
           }

ghcvmProfOptions :: BuildInfo -> [String]
ghcvmProfOptions bi =
  hcProfOptions GHC bi `mappend` hcProfOptions GHCVM bi

ghcvmSharedOptions :: BuildInfo -> [String]
ghcvmSharedOptions bi =
  hcSharedOptions GHC bi `mappend` hcSharedOptions GHCVM bi

-- TODO: Correct default?
isDynamic :: Compiler -> Bool
isDynamic = const True

supportsDynamicToo :: Compiler -> Bool
supportsDynamicToo = Internal.ghcLookupProperty "Support dynamic-too"

-- findGhcvmGhcVersion :: Verbosity -> FilePath -> IO (Maybe Version)
-- findGhcvmGhcVersion verbosity pgm =
--   findProgramVersion "--numeric-ghc-version" id verbosity pgm

-- findGhcvmPkgGhcvmVersion :: Verbosity -> FilePath -> IO (Maybe Version)
-- findGhcvmPkgGhcvmVersion verbosity pgm =
--   findProgramVersion "--numeric-ghcvm-version" id verbosity pgm

-- -----------------------------------------------------------------------------
-- Registering

hcPkgInfo :: ProgramConfiguration -> HcPkg.HcPkgInfo
hcPkgInfo conf = HcPkg.HcPkgInfo { HcPkg.hcPkgProgram    = ghcvmPkgProg
                                 , HcPkg.noPkgDbStack    = False
                                 , HcPkg.noVerboseFlag   = False
                                 , HcPkg.flagPackageConf = False
                                 , HcPkg.useSingleFileDb = False
                                 }
  where
    v                 = versionBranch ver
    Just ghcvmPkgProg = lookupProgram ghcvmPkgProgram conf
    Just ver          = programVersion ghcvmPkgProg

-- | Get the JavaScript file name and command and arguments to run a
--   program compiled by GHCVM
--   the exe should be the base program name without exe extension
runCmd :: ProgramConfiguration -> FilePath
            -> (FilePath, FilePath, [String])
runCmd conf exe =
  ( script
  , programPath ghcvmProg
  , programDefaultArgs ghcvmProg ++ programOverrideArgs ghcvmProg ++ ["--run"]
  )
  where
    script = exe <.> "jsexe" </> "all" <.> "js"
    Just ghcvmProg = lookupProgram ghcvmProgram conf

-- NOTE: GHCVM is frozen after 7.10.3
ghcvmGhcVersion :: Version
ghcvmGhcVersion = makeVersion [7,10,3]
