module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (find, fold, foldMap, head, length, nub, snoc, sort)
import Data.Either (Either(..), hush)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..), joinWith, null, split, stripPrefix)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Console (error, log)
import Node.Buffer (toString)
import Node.ChildProcess (ChildProcess, defaultExecOptions, defaultExecSyncOptions, exec, execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Node.Path (FilePath, concat)
import Node.Platform (Platform(..))
import Node.Process (exit, platform)
import Types (PackageName, mkPackageName)

-- TODO probably there's a better impl for which
which :: String -> Effect (Maybe String)
which cmd = do
  out <- execSync (wh <> " " <> cmd) defaultExecSyncOptions >>= toString UTF8
  pure $ filterEmpty $ head $ split (Pattern "\n") out 
  where
    win32 = maybe false (_ == Win32) platform
    wh = if win32 then "where" else "which"
    filterEmpty Nothing = Nothing
    filterEmpty (Just x) = if null x then Nothing else Just x

type PackageConfig =
  { name    :: PackageName
  , depends :: Array PackageName
  , set     :: String
  , source  :: String
  }
-- derive instance genericPackageConfig :: Generic PackageConfig _
-- derive instance encodeJsonPackageConfig :: EncodeJson PackageConfig
-- instance encodeJsonPackageConfig :: EncodeJson PackageConfig where encodeJson = genericEncodeJson
-- instance decodeJsonPackageConfig :: DecodeJson PackageConfig where decodeJson = genericDecodeJson

type PackageInfo =
  { repo         :: String
  , version      :: String
  , dependencies :: Array PackageName
  }

type PackageSet = Map.Map PackageName PackageInfo

echo :: String -> Effect Unit
echo msg = log (msg <> "\n")

exitWithErr :: forall a. String -> Effect a
exitWithErr msg = err msg *> exit 1
  where err = split (Pattern "\n") >>> traverse error

packageFile :: FilePath
packageFile = "psc-package.json"

localPackageSet :: FilePath
localPackageSet = "packages.json"

packageDir :: String -> PackageName -> String -> FilePath
packageDir set pkgName version = concat [".psc-package", set, (unwrap pkgName), version]

readPackageFile :: Effect PackageConfig
readPackageFile = do
  e <- exists packageFile
  unless e $ exitWithErr "psc-package.json does not exist. Maybe you need to run psc-package init?"
  mpkg <- (decodeJson <=< jsonParser) <$> readTextFile UTF8 packageFile
  case mpkg of
    Left errors -> exitWithErr $ "Unable to parse psc-package.json: " <> errors
    Right pkg -> pure pkg

packageConfigToJSON :: PackageConfig -> String
packageConfigToJSON =
    -- TODO pretty print JSON
    -- config = defConfig
    --            { confCompare =
    --                keyOrder [ "name"
    --                         , "set"
    --                         , "source"
    --                         , "depends"
    --                         ]
    --            , confIndent = Spaces 2
    --            , confTrailingNewline = True
    --            }
    stringify <<< encodeJson <<< sortDependencies
  where
    sortDependencies :: PackageConfig -> PackageConfig
    sortDependencies pkgc = pkgc { depends = sort pkgc.depends }

packageSetToJSON :: PackageSet -> String
packageSetToJSON =
    -- TODO pretty print json
    -- config = defConfig
    --            { confCompare = compare
    --            , confIndent = Spaces 2
    --            , confTrailingNewline = True
    --            }
    stringify <<< encodeJson <<< map sortDependencies
  where
    sortDependencies pkgs = pkgs { dependencies = sort pkgs.dependencies }

writePackageFile :: PackageConfig -> Effect Unit
writePackageFile =
  writeTextFile UTF8 packageFile <<< packageConfigToJSON

cloneShallow
  :: String   -- repo
  -> String   -- branch/tag
  -> FilePath -- target directory
  -> Effect ChildProcess
cloneShallow from ref into =
  -- TODO make sure this runs correctly, might need to switch to Sync or resolve promises
  exec
    ( joinWith " "
    [ "git", "clone"
    , "-q"
    , "-c", "advice.detachedHead=false"
    , "--depth", "1"
    , "-b", ref
    , from
    , into
    ]) defaultExecOptions \{error, stderr, stdout} ->
      pure unit

listRemoteTags
  :: String -- repo
  -> Effect String
listRemoteTags from = execSync
  -- TODO make sure this isn't slow, might need to switch to Async
  ( joinWith " "
  [ "git", "ls-remote"
  , "-q"
  , "-t"
  , from
  ]) defaultExecSyncOptions >>= toString UTF8

getPackageSet :: PackageConfig -> Effect Unit
getPackageSet { source, set } = do
  let pkgDir = concat [".psc-package", set, ".set"]
  e <- exists pkgDir
  unless e <<< void $ cloneShallow source set pkgDir

handleReadPackageSet :: FilePath -> Effect PackageSet
handleReadPackageSet dbFile = do
  e <- exists dbFile
  unless e $ exitWithErr $ dbFile <> " does not exist"
  mdb <- (decodeJson <=< jsonParser) <$> readTextFile UTF8 dbFile
  case mdb of
    Left errors -> exitWithErr $ "Unable to parse packages.json: " <> errors
    Right db -> pure db

readPackageSet :: PackageConfig -> Effect PackageSet
readPackageSet { set } = do
  let dbFile = concat [".psc-package", set, ".set", "packages.json"]
  handleReadPackageSet dbFile

writePackageSet :: PackageConfig -> PackageSet -> Effect Unit
writePackageSet { set } =
  let dbFile = concat [".psc-package", set, ".set", "packages.json"]
  in writeTextFile UTF8 dbFile <<< packageSetToJSON

readLocalPackageSet :: Effect PackageSet
readLocalPackageSet = handleReadPackageSet localPackageSet

writeLocalPackageSet :: PackageSet -> Effect Unit
writeLocalPackageSet = writeTextFile UTF8 localPackageSet <<< packageSetToJSON

performInstall :: String -> PackageName -> PackageInfo -> Effect FilePath
performInstall set pkgName { repo, version } = do
  let pkgDir = packageDir set pkgName version
  e <- exists pkgDir
  unless e <<< void $ do
    echo ("Installing " <> unwrap pkgName)
    cloneShallow repo version pkgDir
  pure pkgDir

getReverseDeps  :: PackageSet -> PackageName -> Effect (Array (Tuple PackageName PackageInfo))
getReverseDeps db dep =
    nub <$> foldMap go (Map.toUnfoldable db :: Array (Tuple PackageName PackageInfo))
  where
    go :: (Tuple PackageName PackageInfo) -> Effect (Array (Tuple PackageName PackageInfo))
    go pair@(Tuple packageName { dependencies }) =
      case find ((==) dep) dependencies of
        Nothing -> pure mempty
        Just _ -> do
          innerDeps <- getReverseDeps db packageName 
          pure $ snoc innerDeps pair

getPursPath :: Effect String
getPursPath = do
  bin <- which "purs" -- *nix binary
  exe <- which "purs.exe" -- windows binary (maybe it putted directly on the PATH)
  cmd <- which "purs.cmd" -- windows binary wrapper (maybe it built with npm)
  let purs = bin <|> exe <|> cmd
  case purs of
    Nothing -> exitWithErr "The \"purs\" executable could not be found. Please make sure your PATH variable is set correctly"
    Just p -> pure $ p

getTransitiveDeps :: PackageSet -> Array PackageName -> Effect (Array (Tuple PackageName PackageInfo))
getTransitiveDeps db deps =
    Map.toUnfoldable <<< fold <$> traverse (go Set.empty) deps
  where
    go seen pkg
      | pkg `Set.member` seen =
          exitWithErr ("Cycle in package dependencies at package " <> unwrap pkg)
      | otherwise =
        case Map.lookup pkg db of
          Nothing ->
            exitWithErr (pkgNotFoundMsg pkg)
          Just info@{ dependencies } -> do
            m <- fold <$> traverse (go (Set.insert pkg seen)) dependencies
            pure (Map.insert pkg info m)

    pkgNotFoundMsg pkg =
      "Package `" <> unwrap pkg <> "` does not exist in package set" <> extraHelp
      where
        extraHelp = case suggestedPkg of
          Just pkg' | Map.member pkg' db ->
            " (but `" <> unwrap pkg' <> "` does, did you mean that instead?)"
          Just pkg' ->
            " (and nor does `" <> unwrap pkg' <> "`)"
          Nothing ->
            ""

        suggestedPkg = do
          sansPrefix <- stripPrefix (Pattern "purescript-") (unwrap pkg)
          hush (mkPackageName sansPrefix)

installImpl :: PackageConfig -> Maybe Int -> Effect Unit
installImpl config@{ depends } limitJobs = do
  getPackageSet config
  db <- readPackageSet config
  newPkgs <- getNewPackages db
  when (length newPkgs > 1) $ do
    echo ("Installing " <> show (length newPkgs) <> " new packages...")
    void $ traverse (uncurry $ performInstall config.set) newPkgs
    -- TODO: this definitely need to be concurrent
  -- case limitJobs of
  --   Nothing ->
  --     traverse newPkgs <<< uncurry $ performInstall config.set
  --   Just max' -> do
  --     sem <- newQSem max'
  --     forConcurrently_ newPkgs <<< uncurry <<< (\x y z -> bracket_ (waitQSem sem) (signalQSem sem) (performInstall x y z)) $ set config
  where
    getNewPackages db =
      getTransitiveDeps db depends-- >>= filterM isNewPackage

    isNewPackage (Tuple name info) =
      map not $ exists $ packageDir config.set name info.version










main :: Effect Unit
main = do
  log "Hello sailor!"

