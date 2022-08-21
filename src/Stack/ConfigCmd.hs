{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecordWildCards     #-}

-- | Make changes to project or global configuration.
module Stack.ConfigCmd
       (cfgCmdName

       -- * config dump project
       ,ConfigCmdDumpProject(..)
       ,configCmdDumpProjectParser
       ,cfgCmdDumpProject
       ,cfgCmdDumpProjectName

       -- * config dump stack
       ,ConfigCmdDumpStack(..)
       ,configCmdDumpStackParser
       ,cfgCmdDumpStack
       ,cfgCmdDumpStackName

       -- * config get
       ,ConfigCmdGet(..)
       ,configCmdGetParser
       ,cfgCmdGet
       ,cfgCmdGetName

       -- * config set
       ,ConfigCmdSet(..)
       ,configCmdSetParser
       ,cfgCmdSet
       ,cfgCmdSetName

       -- * config env
       ,configCmdEnvParser
       ,cfgCmdEnv
       ,cfgCmdEnvName
       ) where

import           Stack.Prelude
#if MIN_VERSION_aeson(2,0,0)
import           Pantry.Internal.AesonExtended
                 (ToJSON(..), FromJSON, (.=), WithJSONWarnings (WithJSONWarnings), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
#endif
import           Data.ByteString.Builder (byteString)
import qualified Data.Map.Merge.Strict as Map
#if !MIN_VERSION_aeson(2,0,0)
import qualified Data.HashMap.Strict as HMap
#endif
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Options.Applicative.Builder.Extra
import           Pantry (loadSnapshot)
import           Path
import qualified RIO.Map as Map
import           RIO.Process (envVarsL)
import           Stack.Config (makeConcreteResolver, getProjectConfig, getImplicitGlobalProjectDir)
import           Stack.Constants
import           Stack.Types.Config
import           Stack.Types.Resolver
import           System.Environment (getEnvironment)

data ConfigDumpFormat = ConfigDumpYaml | ConfigDumpJson

-- | Dump project configuration settings.
data ConfigCmdDumpProject = ConfigCmdDumpProject CommandScope ConfigDumpFormat

-- | Dump stack's own settings. Configuration related to its own opertion. This
-- can be defaulted or stored in a global location or project location or both,
-- in @~\/.stack\/config.yaml@ or @stack.yaml@.
data ConfigCmdDumpStack = ConfigCmdDumpStack DumpStackScope ConfigDumpFormat

-- | Get configuration items that can be individually set by `stack config set`.
data ConfigCmdGet
    = ConfigCmdGetResolver
    | ConfigCmdGetSystemGhc CommandScope
    | ConfigCmdGetInstallGhc CommandScope

-- | Set the resolver for the project or set configuration aronud GHC at project
-- or global scope.
data ConfigCmdSet
    = ConfigCmdSetResolver (Unresolved AbstractResolver)
    | ConfigCmdSetSystemGhc CommandScope Bool
    | ConfigCmdSetInstallGhc CommandScope Bool

-- | Where to get the configuration settings from.
data CommandScope
    = CommandScopeGlobal
      -- ^ Apply changes to or get settings from the global configuration,
      -- typically at @~\/.stack\/config.yaml@.
    | CommandScopeProject
      -- ^ Apply changes to or get settings from the project @stack.yaml@.

-- | Where to get the configuration settings from.
data DumpStackScope
    = DumpStackScopeEffective
      -- ^ The effective scope.
    | DumpStackScopeGlobal
      -- ^ Apply changes to or get settings from the global configuration,
      -- typically at @~\/.stack\/config.yaml@.
    | DumpStackScopeProject
      -- ^ Apply changes to or get settings from the project @stack.yaml@.

instance Display CommandScope where
    display CommandScopeProject = "project"
    display CommandScopeGlobal = "global"

configCmdDumpProjectScope :: ConfigCmdDumpProject -> CommandScope
configCmdDumpProjectScope (ConfigCmdDumpProject scope _) = scope

configCmdGetScope :: ConfigCmdGet -> CommandScope
configCmdGetScope ConfigCmdGetResolver = CommandScopeProject
configCmdGetScope (ConfigCmdGetSystemGhc scope) = scope
configCmdGetScope (ConfigCmdGetInstallGhc scope) = scope

configCmdSetScope :: ConfigCmdSet -> CommandScope
configCmdSetScope (ConfigCmdSetResolver _) = CommandScopeProject
configCmdSetScope (ConfigCmdSetSystemGhc scope _) = scope
configCmdSetScope (ConfigCmdSetInstallGhc scope _) = scope

encodeDumpProject :: ConfigDumpFormat -> (Project -> ByteString)
encodeDumpProject ConfigDumpYaml = Yaml.encode
encodeDumpProject ConfigDumpJson = toStrictBytes . Aeson.encode

encodeDumpStackBy :: ToJSON a => (Config -> a) -> ConfigCmdDumpStack -> (Config -> ByteString)
encodeDumpStackBy f (ConfigCmdDumpStack _ ConfigDumpYaml) = Yaml.encode . f
encodeDumpStackBy f (ConfigCmdDumpStack _ ConfigDumpJson) = toStrictBytes . Aeson.encode . f

encodeDumpStack :: ConfigDumpFormat -> (DumpStack -> ByteString)
encodeDumpStack ConfigDumpYaml = Yaml.encode
encodeDumpStack ConfigDumpJson = toStrictBytes . Aeson.encode

cfgReadProject :: (HasConfig env, HasLogFunc env) => CommandScope -> RIO env (Maybe Project)
cfgReadProject scope = do
    (configFilePath, yamlConfig) <- cfgRead scope
    let parser = parseProjectAndConfigMonoid (parent configFilePath)
    case Yaml.parseEither parser yamlConfig of
        Left err -> do
            logError . display $ T.pack err
            return Nothing
        Right (WithJSONWarnings res _warnings) -> do
            ProjectAndConfigMonoid project _ <- liftIO res
            return $ Just project

cfgCmdDumpProject :: (HasConfig env, HasLogFunc env) => ConfigCmdDumpProject -> RIO env ()
cfgCmdDumpProject cmd@(ConfigCmdDumpProject _ dumpFormat) = do
    project <- cfgReadProject (configCmdDumpProjectScope cmd)
    project & maybe (logError "Couldn't find project") (\p ->
        encodeDumpProject dumpFormat p
        & decodeUtf8'
        & either throwM (logInfo . display))

data DumpStack =
    DumpStack
        { dsInstallGHC :: !(Maybe Bool)
        , dsSystemGHC  :: !(Maybe Bool)
        }

instance ToJSON DumpStack where
    toJSON DumpStack{..} = object
        [ "install-GHC" .= toJSON dsInstallGHC
        , "system-GHC" .= toJSON dsSystemGHC
        ]

cfgCmdDumpStack :: (HasConfig env, HasLogFunc env) => ConfigCmdDumpStack -> RIO env ()
cfgCmdDumpStack cmd@(ConfigCmdDumpStack scope dumpFormat)
    | DumpStackScopeEffective <- scope = cfgCmdDumpStackEffective cmd
    | DumpStackScopeProject <- scope = cfgDumpStack CommandScopeProject dumpFormat
    | DumpStackScopeGlobal <- scope = cfgDumpStack CommandScopeGlobal dumpFormat

cfgDumpStack
    :: (HasConfig env, HasLogFunc env)
    => CommandScope -> ConfigDumpFormat -> RIO env ()
cfgDumpStack scope dumpFormat = do
    (configFilePath, yamlConfig) <- cfgRead scope
    let parser = parseConfigMonoid (parent configFilePath)
    case Yaml.parseEither parser yamlConfig of
        Left err -> logError . display $ T.pack err
        Right (WithJSONWarnings config _warnings) -> do
            let dsSystemGHC = getFirst $ configMonoidSystemGHC config
            let dsInstallGHC = getFirstTrue $ configMonoidInstallGHC config
            
            DumpStack{..}
                & encodeDumpStack dumpFormat
                & decodeUtf8'
                & either throwM (logInfo . display)

cfgCmdDumpStackEffective :: (HasConfig env, HasLogFunc env) => ConfigCmdDumpStack -> RIO env ()
cfgCmdDumpStackEffective cmd = do
    conf <- view configL
    let f Config{..} =
            DumpStack
                { dsInstallGHC = Just configInstallGHC
                , dsSystemGHC = Just configSystemGHC
                }
    conf
        & encodeDumpStackBy f cmd
        & decodeUtf8'
        & either throwM (logInfo . display)

cfgCmdGet :: (HasConfig env, HasLogFunc env) => ConfigCmdGet -> RIO env ()
cfgCmdGet cmd = do
    let logBool maybeValue = logInfo $
            maybe "default" (display . T.toLower . T.pack . show) maybeValue

    (configFilePath, yamlConfig) <- cfgRead (configCmdGetScope cmd)
    let parser = parseProjectAndConfigMonoid (parent configFilePath)
    case Yaml.parseEither parser yamlConfig of
        Left err -> logError . display $ T.pack err
        Right (WithJSONWarnings res _warnings) -> do
            ProjectAndConfigMonoid project config <- liftIO res
            cmd & \case
                ConfigCmdGetResolver ->
                    logInfo . display $ projectResolver project
                ConfigCmdGetSystemGhc{} ->
                    logBool (getFirst $ configMonoidSystemGHC config)
                ConfigCmdGetInstallGhc{} ->
                    logBool (getFirstTrue $ configMonoidInstallGHC config)

-- | Configuration location for a scope. Typically:
-- * at @~\/.stack\/config.yaml@ for global scope.
-- * at @.\/stack.yaml@ by default or from the @--stack-yaml@ option for project scope.
cfgLocation :: HasConfig s => CommandScope -> RIO s (Path Abs File)
cfgLocation scope = do
    conf <- view configL
    case scope of
        CommandScopeProject -> do
            mstackYamlOption <- view $ globalOptsL.to globalStackYaml
            mstackYaml <- getProjectConfig mstackYamlOption
            case mstackYaml of
                PCProject stackYaml -> return stackYaml
                PCGlobalProject -> liftM (</> stackDotYaml) (getImplicitGlobalProjectDir conf)
                PCNoProject _extraDeps ->
                    -- REVIEW: Maybe modify the ~/.stack/config.yaml file instead?
                    throwString "config command used when no project configuration available"
        CommandScopeGlobal -> return (configUserConfigPath conf)

cfgRead :: (HasConfig s, FromJSON a) => CommandScope -> RIO s (Path Abs File, a)
cfgRead scope = do
    configFilePath <- cfgLocation scope

    -- We don't need to worry about checking for a valid yaml here
    liftIO (Yaml.decodeFileEither (toFilePath configFilePath)) >>=
        either throwM (return . (configFilePath,))

cfgCmdSet :: (HasConfig env, HasGHCVariant env) => ConfigCmdSet -> RIO env ()
cfgCmdSet cmd = do
    (configFilePath, config) <- cfgRead (configCmdSetScope cmd)
    newValue <- cfgCmdSetValue (parent configFilePath) cmd
    let cmdKey = cfgCmdSetOptionName cmd
#if MIN_VERSION_aeson(2,0,0)
        config' = KeyMap.insert (Key.fromText cmdKey) newValue config
#else
        config' = HMap.insert cmdKey newValue config
#endif
    if config' == config
        then logInfo
                 (fromString (toFilePath configFilePath) <>
                  " already contained the intended configuration and remains unchanged.")
        else do
            writeBinaryFileAtomic configFilePath (byteString (Yaml.encode config'))
            logInfo (fromString (toFilePath configFilePath) <> " has been updated.")

cfgCmdSetValue
    :: (HasConfig env, HasGHCVariant env)
    => Path Abs Dir -- ^ root directory of project
    -> ConfigCmdSet -> RIO env Yaml.Value
cfgCmdSetValue root (ConfigCmdSetResolver newResolver) = do
    newResolver' <- resolvePaths (Just root) newResolver
    concreteResolver <- makeConcreteResolver newResolver'
    -- Check that the snapshot actually exists
    void $ loadSnapshot =<< completeSnapshotLocation concreteResolver
    return (Yaml.toJSON concreteResolver)
cfgCmdSetValue _ (ConfigCmdSetSystemGhc _ bool') =
    return (Yaml.Bool bool')
cfgCmdSetValue _ (ConfigCmdSetInstallGhc _ bool') =
    return (Yaml.Bool bool')

cfgCmdSetOptionName :: ConfigCmdSet -> Text
cfgCmdSetOptionName (ConfigCmdSetResolver _) = "resolver"
cfgCmdSetOptionName (ConfigCmdSetSystemGhc _ _) = configMonoidSystemGHCName
cfgCmdSetOptionName (ConfigCmdSetInstallGhc _ _) = configMonoidInstallGHCName

cfgCmdName, cfgCmdGetName, cfgCmdSetName, cfgCmdEnvName :: String
cfgCmdDumpProjectName, cfgCmdDumpStackName :: String
cfgCmdName = "config"
cfgCmdDumpProjectName = "dump-project"
cfgCmdDumpStackName = "dump-stack"
cfgCmdGetName = "get"
cfgCmdSetName = "set"
cfgCmdEnvName = "env"

configCmdDumpProjectParser :: OA.Parser ConfigCmdDumpProject
configCmdDumpProjectParser = ConfigCmdDumpProject <$> getScopeFlag <*> dumpFormatFlag

configCmdDumpStackParser :: OA.Parser ConfigCmdDumpStack
configCmdDumpStackParser = ConfigCmdDumpStack <$> getDumpStackScope <*> dumpFormatFlag

dumpFormatFlag :: OA.Parser ConfigDumpFormat
dumpFormatFlag =
    OA.flag
        ConfigDumpYaml
        ConfigDumpJson
            (OA.long "json" <> OA.help "Dump the configuration as JSON instead of as YAML")

configCmdGetParser :: OA.Parser ConfigCmdGet
configCmdGetParser =
    OA.hsubparser $
    mconcat
        [ OA.command
              "resolver"
              (OA.info
                   (OA.pure ConfigCmdGetResolver)
                   (OA.progDesc "Gets the configured resolver."))
        , OA.command
              (T.unpack configMonoidSystemGHCName)
              (OA.info
                   (ConfigCmdGetSystemGhc <$> getScopeFlag)
                   (OA.progDesc
                        "Gets whether stack should use a system GHC installation or not."))
        , OA.command
              (T.unpack configMonoidInstallGHCName)
              (OA.info
                   (ConfigCmdGetInstallGhc <$> getScopeFlag)
                   (OA.progDesc
                        "Gets whether stack should automatically install GHC when necessary."))
        ]

configCmdSetParser :: OA.Parser ConfigCmdSet
configCmdSetParser =
    OA.hsubparser $
    mconcat
        [ OA.command
              "resolver"
              (OA.info
                   (ConfigCmdSetResolver <$>
                    OA.argument
                        readAbstractResolver
                        (OA.metavar "RESOLVER" <>
                         OA.help "E.g. \"nightly\" or \"lts-7.2\""))
                   (OA.progDesc
                        "Change the resolver of the current project. See https://docs.haskellstack.org/en/stable/yaml_configuration/#resolver for more info."))
        , OA.command
              (T.unpack configMonoidSystemGHCName)
              (OA.info
                   (ConfigCmdSetSystemGhc <$> setScopeFlag <*> boolArgument)
                   (OA.progDesc
                        "Configure whether stack should use a system GHC installation or not."))
        , OA.command
              (T.unpack configMonoidInstallGHCName)
              (OA.info
                   (ConfigCmdSetInstallGhc <$> setScopeFlag <*> boolArgument)
                   (OA.progDesc
                        "Configure whether stack should automatically install GHC when necessary."))
        ]

getScopeFlag, setScopeFlag :: OA.Parser CommandScope
getScopeFlag = scopeFlag "From"
setScopeFlag = scopeFlag "Modify"

getDumpStackScope :: OA.Parser DumpStackScope
getDumpStackScope = OA.option readDumpStackScope
    $ OA.long "lens"
    <> OA.help "Which configuration to look at, project or global or effective (global with project overrides)."
    <> OA.metavar "[project|global|effective]"

scopeFlag :: String -> OA.Parser CommandScope
scopeFlag action =
    OA.flag
        CommandScopeProject
        CommandScopeGlobal
        (OA.long "global" <>
         OA.help
             (action <> " the global configuration (typically at \"~/.stack/config.yaml\") instead of the project stack.yaml."))

readDumpStackScope :: OA.ReadM DumpStackScope
readDumpStackScope = OA.str >>= \case
    ("effective" :: String) -> return DumpStackScopeEffective
    "project" -> return DumpStackScopeProject
    "global" -> return DumpStackScopeGlobal
    _ -> OA.readerError "Accepted scopes are 'effective', 'project' and 'global'."

readBool :: OA.ReadM Bool
readBool = OA.readerAsk >>= \case
    "true" -> return True
    "false" -> return False
    s -> OA.readerError ("Invalid value " ++ show s ++ ": Expected \"true\" or \"false\"")

boolArgument :: OA.Parser Bool
boolArgument = OA.argument readBool (OA.metavar "true|false" <> OA.completeWith ["true", "false"])

configCmdEnvParser :: OA.Parser EnvSettings
configCmdEnvParser = EnvSettings
  <$> boolFlags True "locals" "include local package information" mempty
  <*> boolFlags True "ghc-package-path" "set GHC_PACKAGE_PATH variable" mempty
  <*> boolFlags True "stack-exe" "set STACK_EXE environment variable" mempty
  <*> boolFlags False "locale-utf8" "set the GHC_CHARENC environment variable to UTF8" mempty
  <*> boolFlags False "keep-ghc-rts" "keep any GHC_RTS environment variables" mempty

data EnvVarAction = EVASet !Text | EVAUnset
  deriving Show

cfgCmdEnv :: EnvSettings -> RIO EnvConfig ()
cfgCmdEnv es = do
  origEnv <- liftIO $ Map.fromList . map (first fromString) <$> getEnvironment
  mkPC <- view $ configL.to configProcessContextSettings
  pc <- liftIO $ mkPC es
  let newEnv = pc ^. envVarsL
      actions = Map.merge
        (pure EVAUnset)
        (Map.traverseMissing $ \_k new -> pure (EVASet new))
        (Map.zipWithMaybeAMatched $ \_k old new -> pure $
            if fromString old == new
              then Nothing
              else Just (EVASet new))
        origEnv
        newEnv
      toLine key EVAUnset = "unset " <> encodeUtf8Builder key <> ";\n"
      toLine key (EVASet value) =
        encodeUtf8Builder key <> "='" <>
        encodeUtf8Builder (T.concatMap escape value) <> -- TODO more efficient to use encodeUtf8BuilderEscaped
        "'; export " <>
        encodeUtf8Builder key <> ";\n"
      escape '\'' = "'\"'\"'"
      escape c = T.singleton c
  hPutBuilder stdout $ Map.foldMapWithKey toLine actions