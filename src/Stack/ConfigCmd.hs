{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}

-- | Make changes to project or global configuration.
module Stack.ConfigCmd
       (cfgCmdName

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
                 (FromJSON, WithJSONWarnings (WithJSONWarnings))
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

data ConfigCmdGet
    = ConfigCmdGetResolver
    | ConfigCmdGetSystemGhc CommandScope
    | ConfigCmdGetInstallGhc CommandScope

data ConfigCmdSet
    = ConfigCmdSetResolver (Unresolved AbstractResolver)
    | ConfigCmdSetSystemGhc CommandScope Bool
    | ConfigCmdSetInstallGhc CommandScope Bool

data CommandScope
    = CommandScopeGlobal
      -- ^ Apply changes to the global configuration,
      --   typically at @~/.stack/config.yaml@.
    | CommandScopeProject
      -- ^ Apply changes to the project @stack.yaml@.

configCmdGetScope :: ConfigCmdGet -> CommandScope
configCmdGetScope ConfigCmdGetResolver = CommandScopeProject
configCmdGetScope (ConfigCmdGetSystemGhc scope) = scope
configCmdGetScope (ConfigCmdGetInstallGhc scope) = scope

configCmdSetScope :: ConfigCmdSet -> CommandScope
configCmdSetScope (ConfigCmdSetResolver _) = CommandScopeProject
configCmdSetScope (ConfigCmdSetSystemGhc scope _) = scope
configCmdSetScope (ConfigCmdSetInstallGhc scope _) = scope

cfgRead :: (HasConfig s, FromJSON a) => CommandScope -> RIO s (Path Abs File, a)
cfgRead scope = do
    conf <- view configL
    configFilePath <-
             case scope of
                 CommandScopeProject -> do
                     mstackYamlOption <- view $ globalOptsL.to globalStackYaml
                     mstackYaml <- getProjectConfig mstackYamlOption
                     case mstackYaml of
                         PCProject stackYaml -> return stackYaml
                         PCGlobalProject -> liftM (</> stackDotYaml) (getImplicitGlobalProjectDir conf)
                         PCNoProject _extraDeps -> throwString "config command used when no project configuration available" -- maybe modify the ~/.stack/config.yaml file instead?
                 CommandScopeGlobal -> return (configUserConfigPath conf)

    -- We don't need to worry about checking for a valid yaml here
    liftIO (Yaml.decodeFileEither (toFilePath configFilePath)) >>=
        either throwM (return . (configFilePath,))

cfgCmdGet :: (HasConfig env, HasLogFunc env) => ConfigCmdGet -> RIO env ()
cfgCmdGet cmd = do
    (configFilePath, yamlConfig) <- cfgRead (configCmdGetScope cmd)
    let parser = parseProjectAndConfigMonoid (parent configFilePath)
    case Yaml.parseEither parser yamlConfig of
        Left err -> logError . display $ T.pack err
        Right (WithJSONWarnings res _warnings) -> do
            ProjectAndConfigMonoid project config <- liftIO res
            cmd & \case
                ConfigCmdGetResolver -> do
                    logInfo $ "resolver: " <> display (projectResolver project)

                ConfigCmdGetSystemGhc CommandScopeProject ->
                    logBool "system-ghc" (getFirst $ configMonoidSystemGHC config)

                ConfigCmdGetSystemGhc CommandScopeGlobal -> do
                    logInfo "CONFIG GET SYSTEMGHC"

                ConfigCmdGetInstallGhc CommandScopeProject ->
                    logBool "install-ghc" (getFirstTrue $ configMonoidInstallGHC config)

                ConfigCmdGetInstallGhc CommandScopeGlobal -> do
                    logInfo "CONFIG GET INSTALLGHC"
    where
        logBool key maybeValue = logInfo $ key <> " :" <>
            maybe "default" (display . T.toLower . T.pack . show) maybeValue

cfgCmdSet
    :: (HasConfig env, HasGHCVariant env)
    => ConfigCmdSet -> RIO env ()
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

cfgCmdName :: String
cfgCmdName = "config"

cfgCmdGetName :: String
cfgCmdGetName = "get"

cfgCmdSetName :: String
cfgCmdSetName = "set"

cfgCmdEnvName :: String
cfgCmdEnvName = "env"

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
                   (ConfigCmdGetSystemGhc <$> scopeFlag)
                   (OA.progDesc
                        "Gets whether stack should use a system GHC installation or not."))
        , OA.command
              (T.unpack configMonoidInstallGHCName)
              (OA.info
                   (ConfigCmdGetInstallGhc <$> scopeFlag)
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
                   (ConfigCmdSetSystemGhc <$> scopeFlag <*> boolArgument)
                   (OA.progDesc
                        "Configure whether stack should use a system GHC installation or not."))
        , OA.command
              (T.unpack configMonoidInstallGHCName)
              (OA.info
                   (ConfigCmdSetInstallGhc <$> scopeFlag <*> boolArgument)
                   (OA.progDesc
                        "Configure whether stack should automatically install GHC when necessary."))
        ]

scopeFlag :: OA.Parser CommandScope
scopeFlag =
    OA.flag
        CommandScopeProject
        CommandScopeGlobal
        (OA.long "global" <>
         OA.help
             "Modify the global configuration (typically at \"~/.stack/config.yaml\") instead of the project stack.yaml.")

readBool :: OA.ReadM Bool
readBool = do
    s <- OA.readerAsk
    case s of
        "true" -> return True
        "false" -> return False
        _ -> OA.readerError ("Invalid value " ++ show s ++ ": Expected \"true\" or \"false\"")

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
