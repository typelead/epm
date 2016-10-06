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
        getGlobalPackageDB
  ) where

import Distribution.Simple.GHC.ImplInfo ( getImplInfo, ghcvmVersionImplInfo )
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
         , locationPath
         , lookupProgram, addKnownPrograms
         , ghcvmProgram, ghcvmPkgProgram, c2hsProgram, hsc2hsProgram
         , ldProgram, haddockProgram, stripProgram
         , javaProgram, javacProgram )
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
import Data.Maybe               ( catMaybes )
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
  (_, conf4) <- requireProgram verbosity javaProgram conf3
  (_, conf5) <- requireProgram verbosity javacProgram conf4
  return (comp, compPlatform, conf5)

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
  let indices = [ PackageIndex.fromList pkgs | (_, pkgs) <- pkgss ]
  return $! (mconcat indices)

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

  (ghcvmProg, _) <- requireProgram verbosity ghcvmProgram (withPrograms lbi)
  let runGhcvmProg        = runGHC verbosity ghcvmProg comp
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
                                            ghcvmSharedOptions libBi
                      }
      vanillaOpts = vanillaOpts' {
                        ghcOptExtraDefault = toNubListR ["-staticlib"]
                    }
      target = libTargetDir </> mkJarName libName

  unless (forRepl || (null (libModules lib) && null javaSrcs)) $ do
       when isVanillaLib $ runGhcvmProg vanillaOpts
       when isSharedLib  $ runGhcvmProg sharedOpts

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
  let runGhcvmProg = runGHC verbosity ghcvmProg comp

  createDirectoryIfMissingVerbose verbosity True exeDir

  srcMainFile <- findFile (hsSourceDirs exeBi) modPath
  let baseOpts = (componentGhcOptions verbosity lbi exeBi clbi exeDir)
                 `mappend` mempty {
                   ghcOptMode         = toFlag GhcModeMake,
                   ghcOptInputFiles   = toNubListR $ srcMainFile : javaSrcs,
                   ghcOptInputModules = toNubListR $ exeModules exe,
                   ghcOptNumJobs      = numJobs,
                   ghcOptOutputFile   = toFlag exeJar,
                   ghcOptShared       = toFlag isShared
                 }

  runGhcvmProg baseOpts
  -- Generate .sh file
  classPaths' <- if isShared
                then getDependencyClassPaths
                     (installedPkgs lbi)
                     (map fst $ componentPackageDeps clbi)
                else return []
  let classPaths = classPaths'
      generateExeScript = "#!/usr/bin/env sh\n"
                         ++ "DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"\n"
                         ++ "java -classpath \"$DIR/" ++ exeNameReal
                         ++ (if null classPaths then "" else ':' : intercalate ":" classPaths)
                         ++ "\" ghcvm.main\n"
      scriptFile = targetDir </> (exeNameReal -<.> "sh")
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
        javaSrcs = javaSources exeBi
        targetDir = buildDir lbi </> exeName'
        exeDir    = targetDir </> (exeName' ++ "-tmp")
        exeJar    = targetDir </> exeNameReal

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
      --                ghcOptExtra         = toNubListR (ghcvmProfOptions libBi)
      --            }
      ghcArgs = if withVanillaLib lbi then vanillaArgs
--           else if withProfLib    lbi then profArgs
                else error "libAbiHash: Can't find an enabled library way"
  --
  (ghcvmProg, _) <- requireProgram verbosity ghcvmProgram (withPrograms lbi)
  getProgramInvocationOutput verbosity (ghcInvocation ghcvmProg comp ghcArgs)

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
      `mappend` toNubListR (["-pgmjavac", javacPath] ++  (hcOptions GHCVM bi))
  }
  where Just javacProg = lookupProgram javacProgram (withPrograms lbi)
        javacPath = locationPath (programLocation javacProg)

-- ghcvmProfOptions :: BuildInfo -> [String]
-- ghcvmProfOptions bi =
--   hcProfOptions GHC bi `mappend` hcProfOptions GHCVM bi

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

-- NOTE: GHCVM is frozen after 7.10.3
ghcvmGhcVersion :: Version
ghcvmGhcVersion = makeVersion [7,10,3]

jarExtension :: String
jarExtension = "jar"

getDependencyClassPaths :: InstalledPackageIndex -> [InstalledPackageId] -> IO [FilePath]
getDependencyClassPaths packageIndex packages = fmap concat $ mapM hsLibraryPaths packageInfos
  where Left closurePackageIndex = dependencyClosure packageIndex packages
        packageInfos = allPackages closurePackageIndex
        hsLibraryPaths pi = mapM (findFile (libraryDirs pi))
                                 (map (<.> "jar") $ hsLibraries pi)
