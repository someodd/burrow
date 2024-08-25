
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- TODO when base upgrade: {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Customization options and possibly internationalization.

Please consult the example configuration file for details.

The configuration file used when building a gopherhole.

The user can use the configuration file to tweak various settings
altering the gopherhole building process.

-}
module Config where

import Data.Text (Text)
import GHC.Generics (Generic)
import Toml
import System.Directory (canonicalizePath)
import System.FilePath (splitPath, joinPath)
import Data.List (findIndex, isSuffixOf)

-- | The name of the config file a gopherhole uses...
burrowGopherholeDefaultConfigPath :: FilePath
burrowGopherholeDefaultConfigPath = "data/burrow.toml"

{- | Get the config file from path and also specify the "project root" as
something relative to the config file or the current working directory.

The config file resides in a directory called data/ in the project root.

-}
getConfigSpecial
  :: Maybe FilePath
  -- ^ path to the data/burrow.toml file
  -> Bool
  -- ^ Project root is the current working directory?
  -> IO (Config.Config, FilePath, FilePath)
  -- ^ (the config, path to config, project root) -- absolute paths.
getConfigSpecial maybeConfigPath cwdProjectRoot = do
  absConfigPath <- maybe (return Config.burrowGopherholeDefaultConfigPath) canonicalizePath maybeConfigPath

  if cwdProjectRoot
    then do
      projectRoot <- canonicalizePath "."
      config <- Config.getConfig absConfigPath
      pure (config, absConfigPath, projectRoot)
    else
      case ensureInDataDir absConfigPath of
        Nothing -> error $ "Config file must be in a directory named data/, but got: " ++ absConfigPath
        Just projectRoot -> do
          config <- Config.getConfig absConfigPath
          pure (config, absConfigPath, projectRoot)

-- FIXME
{- | Ensure the file is inside a "data/" directory and get the parent directory of "data/"

>>> ensureInDataDir "data/burrow.toml"
Just ""
>>> ensureInDataDir "/wow/data/burrow.toml"
Just "/wow/"
-}
ensureInDataDir :: FilePath -> Maybe FilePath
ensureInDataDir filePath =
  let pathParts = splitPath filePath
      dataIndex = findIndex ("data/" `isSuffixOf`) pathParts
  in case dataIndex of
       Just idx -> Just (joinPath $ take idx pathParts)
       Nothing -> Nothing

{- | Return the config, along with the absolute path it was loaded from.

-}
getConfigOrDefault :: Maybe FilePath -> IO (Config, FilePath)
getConfigOrDefault maybeConfigPath = do
  filePath <- maybe (return burrowGopherholeDefaultConfigPath) canonicalizePath maybeConfigPath
  config <- getConfig filePath
  pure (config, filePath)

data FontsConfig = FontsConfig
  { h1 :: Text
  , h2 :: Text
  , h3 :: Text
  , h4 :: Text
  , h5 :: Text
  , h6 :: Text
  } deriving (Generic, Show, Eq)

fontsConfigCodec :: TomlCodec FontsConfig
fontsConfigCodec = Toml.genericCodec

data PhlogConfig = PhlogConfig
  { phlogPath :: Text
  , tagPath :: Text
  , defaultAuthor :: Text
  } deriving (Generic, Show, Eq)

phlogConfigCodec :: TomlCodec PhlogConfig
phlogConfigCodec = Toml.genericCodec

data GeneralConfig = GeneralConfig
  { buildPath :: Text
  , sourcePath :: Text
  , host :: Text
  , port :: Int
  , buildExtensions :: Text  -- FIXME: list!
  , menuExtensionPrefix :: Text
  , timeFormat :: Text
  , directoryMapName :: Text
  , asciiSafe :: Bool
  } deriving (Generic, Show, Eq)

generalConfigCodec :: TomlCodec GeneralConfig
generalConfigCodec = Toml.genericCodec

data SpacecookieConfig = SpacecookieConfig
  { hostname :: Text
  , listenAddr :: Text
  , listenPort :: Integer
  , user :: Maybe Text
  , root :: Text
  } deriving (Generic, Show, Eq)

spacecookieConfigCodec :: TomlCodec SpacecookieConfig
spacecookieConfigCodec = Toml.genericCodec

data Config = Config
    { fonts :: FontsConfig
    , phlog :: PhlogConfig
    , general :: GeneralConfig
    , spacecookie :: SpacecookieConfig
    }
    deriving (Generic, Show, Eq)

configCodec :: TomlCodec Config
configCodec =
    Config
        <$> Toml.table fontsConfigCodec "fonts" .= fonts
        <*> Toml.table phlogConfigCodec "phlog" .= phlog
        <*> Toml.table generalConfigCodec "general" .= general
        <*> Toml.table spacecookieConfigCodec "spacecookie" .= spacecookie

getConfig :: FilePath -> IO Config
getConfig configFilePath = Toml.decodeFile configCodec configFilePath
