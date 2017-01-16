{-# LANGUAGE CPP #-}

module Distribution.Simple.ETA (
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
        getGlobalPackageDB
  ) where

import Distribution.Simple.GHC.ImplInfo ( getImplInfo, etaVersionImplInfo )
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Package ( InstalledPackageId )
import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), Executable(..)
         , Library(..), libModules, exeModules
         , hcOptions, hcProfOptions, hcSharedOptions
         , allExtensions )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, libraryDirs, hsLibraries )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
                                ( InstalledPackageInfo_(..) )
import Distribution.Simple.PackageIndex ( InstalledPackageIndex, lookupInstalledPackageId,
                                          dependencyClosure, allPackages )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..)
         , LibraryName(..), ComponentName(..) )
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.InstallDirs hiding ( absoluteInstallDirs )
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Simple.Program.Run (programInvocation)
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration
         , ProgramSearchPath
         , rawSystemProgramConf
         , rawSystemProgramStdout, rawSystemProgramStdoutConf
         , getProgramInvocationOutput
         , requireProgramVersion, requireProgram
         , userMaybeSpecifyPath, programPath
         , locationPath
         , lookupProgram, addKnownPrograms
         , etaProgram, etaPkgProgram, c2hsProgram, hsc2hsProgram
         , ldProgram, haddockProgram, stripProgram
         , javaProgram, javacProgram, coursierProgram )
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
         ( Platform(..), OS(..) )
import Distribution.Verbosity
import Distribution.Utils.NubList
         ( overNubListR, toNubListR )
import Distribution.Text ( display )
import Language.Haskell.Extension ( Extension(..)
                                  , KnownExtension(..))

import Control.Monad            ( unless, when )
import Data.Char                ( isSpace )
import Data.List                ( partition, find )
import Data.Maybe               ( catMaybes, fromJust )
import Data.Version             ( makeVersion )
import qualified Data.Map as M  ( fromList  )
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid              ( Monoid(..) )
#endif
import System.Directory         ( doesFileExist, copyFile, setPermissions, getPermissions,
                                  executable )
import System.FilePath          ( (</>), (<.>), (-<.>), takeExtension,
                                  takeDirectory, replaceExtension,
                                  splitExtension )

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration
          -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf0 = do
  (etaProg, etaVersion, conf1) <-
    requireProgramVersion verbosity etaProgram
      anyVersion --(orLaterVersion (Version [0,1] []))
      (userMaybeSpecifyPath "eta" hcPath conf0)

  let implInfo = etaVersionImplInfo etaVersion etaGhcVersion

  -- This is slightly tricky, we have to configure eta first, then we use the
  -- location of eta to help find eta-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (etaPkgProg, etaPkgVersion, conf2) <-
    requireProgramVersion verbosity etaPkgProgram
    {- TODO: Is this necessary? {programFindLocation = guessEtaPkgFromEtaPath etaProg} -}
    anyVersion (userMaybeSpecifyPath "eta-pkg" hcPkgPath conf1)

  -- Just etaPkgEtaVersion <- findEtaPkgEtaVersion
  --                                 verbosity (programPath etaPkgProg)

  when (etaVersion /= etaPkgVersion) $ die $
       "Version mismatch between eta and eta-pkg: "
    ++ programPath etaProg ++ " is version " ++ display etaVersion ++ " "
    ++ programPath etaPkgProg ++ " is version " ++ display etaPkgVersion

  -- when (etaGhcVersion /= etaPkgVersion) $ die $
  --      "Version mismatch between eta and eta-pkg: "
  --   ++ programPath etaProg
  --   ++ " was built with GHC version " ++ display etaGhcVersion ++ " "
  --   ++ programPath etaPkgProg
  --   ++ " was built with GHC version " ++ display etaPkgVersion

  -- be sure to use our versions of hsc2hs, c2hs, haddock and ghc
  -- let hsc2hsProgram' =
  --       hsc2hsProgram { programFindLocation =
  --                         guessHsc2hsFromEtaPath etaProg }
  --     c2hsProgram' =
  --       c2hsProgram { programFindLocation =
  --                         guessC2hsFromEtaPath etaProg }

  --     haddockProgram' =
  --       haddockProgram { programFindLocation =
  --                         guessHaddockFromEtaPath etaProg }
  --     conf3 = addKnownPrograms [ hsc2hsProgram', c2hsProgram', haddockProgram' ] conf2
  let conf3 = conf2 -- TODO: Account for other programs

  languages  <- Internal.getLanguages  verbosity implInfo etaProg
  extensions <- Internal.getExtensions verbosity implInfo etaProg

  etaInfo <- Internal.getGhcInfo verbosity implInfo etaProg
  let etaInfoMap = M.fromList etaInfo

  let comp = Compiler {
        compilerId         = CompilerId ETA etaVersion,
        -- TODO: Make this unique for ETA?
        compilerAbiTag     = AbiTag $
          "ghc" ++ intercalate "_" (map show . versionBranch $ etaGhcVersion),
        compilerCompat     = [CompilerId GHC etaGhcVersion],
        compilerLanguages  = languages,
        compilerExtensions = extensions,
        compilerProperties = etaInfoMap
      }
      compPlatform = Nothing -- Internal.targetPlatform ghcInfo
  (_, conf4) <- requireProgram verbosity javaProgram conf3
  (_, conf5) <- requireProgram verbosity javacProgram conf4
  (_, conf6) <- requireProgram verbosity coursierProgram conf5
  return (comp, compPlatform, conf6)

etaNativeToo :: Compiler -> Bool
etaNativeToo = Internal.ghcLookupProperty "Native Too"

-- guessEtaPkgFromEtaPath :: ConfiguredProgram -> Verbosity
--                            -> ProgramSearchPath -> IO (Maybe FilePath)
-- guessEtaPkgFromEtaPath = guessToolFromEtaPath etaPkgProgram

-- guessHsc2hsFromEtaPath :: ConfiguredProgram -> Verbosity
--                          -> ProgramSearchPath -> IO (Maybe FilePath)
-- guessHsc2hsFromEtaPath = guessToolFromEtaPath hsc2hsProgram

-- guessC2hsFromEtaPath :: ConfiguredProgram -> Verbosity
--                        -> ProgramSearchPath -> IO (Maybe FilePath)
-- guessC2hsFromEtaPath = guessToolFromEtaPath c2hsProgram

-- guessHaddockFromEtaPath :: ConfiguredProgram -> Verbosity
--                           -> ProgramSearchPath -> IO (Maybe FilePath)
-- guessHaddockFromEtaPath = guessToolFromEtaPath haddockProgram

-- guessToolFromEtaPath :: Program -> ConfiguredProgram
--                        -> Verbosity -> ProgramSearchPath
--                        -> IO (Maybe FilePath)
-- guessToolFromEtaPath tool etaProg verbosity searchpath
--   = do let toolname          = programName tool
--            path              = programPath etaProg
--            dir               = takeDirectory path
--            versionSuffix     = takeVersionSuffix (dropExeExtension path)
--            guessNormal       = dir </> toolname <.> exeExtension
--            guessEtaVersioned = dir </> (toolname ++ "-eta" ++ versionSuffix)
--                                  <.> exeExtension
--            guessEta        = dir </> (toolname ++ "-eta")
--                                <.> exeExtension
--            guessVersioned    = dir </> (toolname ++ versionSuffix) <.> exeExtension
--            guesses | null versionSuffix = [guessEta, guessNormal]
--                    | otherwise          = [guessEtaVersioned,
--                                            guessEta,
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
  let indices = [ PackageIndex.fromList pkgs | (_, pkgs) <- pkgss ]
  return $! (mconcat indices)

checkPackageDbEnvVar :: IO ()
checkPackageDbEnvVar =
    Internal.checkPackageDbEnvVar "ETA" "ETA_PACKAGE_PATH"

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
     rawSystemProgramStdoutConf verbosity etaProgram
     (withPrograms lbi) ["--print-libdir"]

getLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
getLibDir' verbosity etaProg =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdout verbosity etaProg ["--print-libdir"]

-- | Return the 'FilePath' to the global GHC package database.
getGlobalPackageDB :: Verbosity -> ConfiguredProgram -> IO FilePath
getGlobalPackageDB verbosity etaProg =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdout verbosity etaProg ["--print-global-package-db"]

toJavaLibName :: String -> String
toJavaLibName lib
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
      isVanillaLib = not forRepl && withVanillaLib lbi
      isSharedLib = not forRepl && withSharedLib lbi
      comp = compiler lbi
      implInfo = getImplInfo comp
      hole_insts = map (\(k,(p,n)) -> (k,(InstalledPackageInfo.packageKey p,n)))
                       (instantiatedWith lbi)

  (etaProg, _) <- requireProgram verbosity etaProgram (withPrograms lbi)
  let runEtaProg        = runGHC verbosity etaProg comp
      libBi               = libBuildInfo lib

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let javaSrcs    = javaSources libBi
      baseOpts    = componentGhcOptions verbosity lbi libBi clbi libTargetDir
      linkJavaLibOpts = mempty {
                          ghcOptInputFiles = toNubListR javaSrcs
                      }
      vanillaOptsNoJavaLib = baseOpts `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptNumJobs      = numJobs,
                      ghcOptPackageKey   = toFlag (pkgKey lbi),
                      ghcOptSigOf        = hole_insts,
                      ghcOptInputModules = toNubListR $ libModules lib,
                      ghcOptOutputFile   = toFlag target
                    }
      vanillaOpts' = vanillaOptsNoJavaLib `mappend` linkJavaLibOpts
      sharedOpts  = vanillaOpts' `mappend` mempty {
                        ghcOptShared = toFlag True,
                        ghcOptExtra       = toNubListR $
                                            etaSharedOptions libBi
                      }
      vanillaOpts = vanillaOpts' {
                        ghcOptExtraDefault = toNubListR ["-staticlib"]
                    }
      target = libTargetDir </> mkJarName libName

  unless (forRepl || (null (libModules lib) && null javaSrcs)) $ do
       when isVanillaLib $ runEtaProg vanillaOpts
       when isSharedLib  $ runEtaProg sharedOpts

-- | Start a REPL without loading any source files.
startInterpreter :: Verbosity -> ProgramConfiguration -> Compiler
                 -> PackageDBStack -> IO ()
startInterpreter verbosity conf comp packageDBs = do
  let replOpts = mempty {
        ghcOptMode       = toFlag GhcModeInteractive,
        ghcOptPackageDBs = packageDBs
        }
  checkPackageDbStack packageDBs
  (etaProg, _) <- requireProgram verbosity etaProgram conf
  runGHC verbosity etaProg comp replOpts

buildExe, replExe :: Verbosity          -> Cabal.Flag (Maybe Int)
                  -> PackageDescription -> LocalBuildInfo
                  -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe = buildOrReplExe False
replExe  = buildOrReplExe True

buildOrReplExe :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildOrReplExe forRepl verbosity numJobs pkgDescr lbi
  exe@Executable { exeName = exeName', modulePath = modPath } clbi = do

  (etaProg, _)      <- requireProgram verbosity etaProgram (withPrograms lbi)
  (coursierProg, _) <- requireProgram verbosity coursierProgram (withPrograms lbi)
  let runEtaProg = runGHC verbosity etaProg comp
      runCoursier options = getProgramInvocationOutput verbosity
                              (programInvocation coursierProg options)

  createDirectoryIfMissingVerbose verbosity True exeDir

  srcMainFile <- findFile (hsSourceDirs exeBi) modPath

  (depJars, mavenDeps') <- getDependencyClassPaths (installedPkgs lbi)
                            pkgDescr lbi clbi

  let mavenDeps = mavenDeps' ++ (extraLibs . buildInfo $ exe)
  mavenOutput <- runCoursier $ "fetch" : mavenDeps

  let mavenPaths = dropWhile ((/= '/') . head) $ lines mavenOutput
      javaSrcs = (if isShared
                  then []
                  else mavenPaths) ++ javaSrcs'
      baseOpts = (componentGhcOptions verbosity lbi exeBi clbi exeDir)
                 `mappend` mempty {
                   ghcOptMode         = toFlag GhcModeMake,
                   ghcOptInputFiles   = toNubListR $ srcMainFile : javaSrcs,
                   ghcOptInputModules = toNubListR $ exeModules exe,
                   ghcOptNumJobs      = numJobs,
                   ghcOptOutputFile   = toFlag exeJar,
                   ghcOptShared       = toFlag isShared
                 }

  runEtaProg baseOpts
  -- Generate .sh file
  let classPaths' = if isShared then depJars ++ mavenPaths else []
      classPaths = (if isShared && not (null javaSrcs)
                    then ["$DIR/" ++ exeName' ++ "-tmp/__extras.jar"]
                    else [])
                    ++ classPaths'
      generateExeScript = "#!/usr/bin/env bash\n"
                         ++ "DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"\n"
                         ++ "java -classpath \"$DIR/" ++ exeNameReal
                         ++ (if null classPaths
                             then ""
                             else classPathSep : intercalate [classPathSep]
                                                   classPaths)
                         ++ "\" eta.main \"$@\"\n"
      scriptFile = targetDir </> exeName'
  writeUTF8File scriptFile generateExeScript
  p <- getPermissions scriptFile
  setPermissions scriptFile (p {executable = True})
  where comp         = compiler lbi
        implInfo     = getImplInfo comp
        exeBi        = buildInfo exe
        exeNameReal = exeName' <.> (if takeExtension exeName' /= ('.':jarExtension)
                                    then jarExtension
                                    else "")
        isShared = withDynExe lbi
        javaSrcs' = javaSources exeBi
        targetDir = buildDir lbi </> exeName'
        exeDir    = targetDir </> (exeName' ++ "-tmp")
        exeJar    = targetDir </> exeNameReal
        isWindows
          | Platform _ Windows <- hostPlatform lbi = True
          | otherwise = False
        classPathSep = if isWindows then ';' else ':'

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
  copyModuleFiles "hi"
  when isVanillaLib $ mapM_ (installOrdinary builtDir targetDir) jarLibNames
  when isSharedLib $ mapM_ (installOrdinary builtDir dynlibTargetDir) jarLibNames
  where
    install isShared srcDir dstDir name = do
      createDirectoryIfMissingVerbose verbosity True dstDir
      installOrdinaryFile   verbosity src dst
      where src = srcDir </> name
            dst = dstDir </> name

    installOrdinary = install False
    installShared   = install True

    copyModuleFiles ext =
      findModuleFiles [builtDir] [ext] (libModules lib)
      >>= installOrdinaryFiles verbosity targetDir

    cid = compilerId (compiler lbi)
    libNames = componentLibraries clbi
    jarLibNames = map mkJarName libNames

    hasLib    = not $ null (libModules lib)
                   && null (javaSources (libBuildInfo lib))
    isVanillaLib = hasLib && withVanillaLib lbi
    isSharedLib  = hasLib && withSharedLib  lbi

mkJarName :: LibraryName -> String
mkJarName (LibraryName lib) = lib <.> "jar"

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
  --print ("installExe", targetDir, dynlibTargetDir, builtDir)
  let binDir = bindir installDirs
      toDir x = binDir </> x
      buildDir = buildPref </> exeName exe
      fromDir x = buildDir </> x
      exeNameExt ext = exeName exe <.> ext
      copy x = copyFile (fromDir x) (toDir x)
  createDirectoryIfMissingVerbose verbosity True binDir
  copy (exeNameExt "sh")
  copy (exeNameExt "jar")
  --copyFile (fromDir (exeNameExt "jar")) (toDir (progprefix ++ exeName exe ++ progsuffix))

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
      --                ghcOptExtra         = toNubListR (etaProfOptions libBi)
      --            }
      ghcArgs = if withVanillaLib lbi then vanillaArgs
--           else if withProfLib    lbi then profArgs
                else error "libAbiHash: Can't find an enabled library way"
  --
  (etaProg, _) <- requireProgram verbosity etaProgram (withPrograms lbi)
  getProgramInvocationOutput verbosity (ghcInvocation etaProg comp ghcArgs)

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

-- TODO: Pass javac as well
componentGhcOptions :: Verbosity -> LocalBuildInfo
                    -> BuildInfo -> ComponentLocalBuildInfo -> FilePath
                    -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
  let opts = Internal.componentGhcOptions verbosity lbi bi clbi odir
  in  opts
  {
    ghcOptExtra = ghcOptExtra opts
      `mappend` toNubListR (["-pgmjavac", javacPath] ++  (hcOptions ETA bi))
  }
  where Just javacProg = lookupProgram javacProgram (withPrograms lbi)
        javacPath = locationPath (programLocation javacProg)

-- etaProfOptions :: BuildInfo -> [String]
-- etaProfOptions bi =
--   hcProfOptions GHC bi `mappend` hcProfOptions ETA bi

etaSharedOptions :: BuildInfo -> [String]
etaSharedOptions bi =
  hcSharedOptions GHC bi `mappend` hcSharedOptions ETA bi

-- TODO: Correct default?
isDynamic :: Compiler -> Bool
isDynamic = const True

supportsDynamicToo :: Compiler -> Bool
supportsDynamicToo = Internal.ghcLookupProperty "Support dynamic-too"

-- findEtaGhcVersion :: Verbosity -> FilePath -> IO (Maybe Version)
-- findEtaGhcVersion verbosity pgm =
--   findProgramVersion "--numeric-ghc-version" id verbosity pgm

-- findEtaPkgEtaVersion :: Verbosity -> FilePath -> IO (Maybe Version)
-- findEtaPkgEtaVersion verbosity pgm =
--   findProgramVersion "--numeric-eta-version" id verbosity pgm

-- -----------------------------------------------------------------------------
-- Registering

hcPkgInfo :: ProgramConfiguration -> HcPkg.HcPkgInfo
hcPkgInfo conf = HcPkg.HcPkgInfo { HcPkg.hcPkgProgram    = etaPkgProg
                                 , HcPkg.noPkgDbStack    = False
                                 , HcPkg.noVerboseFlag   = False
                                 , HcPkg.flagPackageConf = False
                                 , HcPkg.useSingleFileDb = False
                                 }
  where
    v                 = versionBranch ver
    Just etaPkgProg = lookupProgram etaPkgProgram conf
    Just ver          = programVersion etaPkgProg

-- NOTE: ETA is frozen after 7.10.3
etaGhcVersion :: Version
etaGhcVersion = makeVersion [7,10,3]

jarExtension :: String
jarExtension = "jar"

getDependencyClassPaths
  :: InstalledPackageIndex
  -> PackageDescription
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> IO ([FilePath], [String])
getDependencyClassPaths packageIndex pkgDescr lbi clbi = do
  libs <- fmap concat $ mapM hsLibraryPaths packageInfos
  return (libPaths ++ libs, libMavenDeps ++ mavenDeps)
  where mavenDeps = concatMap InstalledPackageInfo.extraLibraries packageInfos
        packages' = map fst $ componentPackageDeps clbi
        (libs, packages'') = partition (isInfixOf "-inplace" . show) packages'
        libPaths = if null libs then [] else ["$DIR/../"
                                              ++ mkJarName (fromJust mbLibName)]
        libMavenDeps = if null libs
                       then []
                       else extraLibs . libBuildInfo . fromJust $ library pkgDescr
        (mbLibName, libDeps) =
          if null libs
          then (Nothing, [])
          else (\(_, clbi, _) ->
                  case componentLibraries clbi of
                    [libName] -> ( Just libName
                                 , map fst $ componentPackageDeps clbi )
                    [] -> error "No library name found when building library"
                    _  -> error "Multiple library names found when building library")
             . fromJust
             . find (\(cn,_,_) -> cn == CLibName)
             $ componentsConfigs lbi

        packages = libDeps ++ packages''

        closurePackageIndex = case dependencyClosure packageIndex packages of
          Left pkgIdx -> pkgIdx
          Right errs -> error $ show ("deps error", errs)

        packageInfos = allPackages closurePackageIndex
        hsLibraryPaths pi = mapM (findFile (libraryDirs pi))
                                 (map (<.> "jar") $ hsLibraries pi)
