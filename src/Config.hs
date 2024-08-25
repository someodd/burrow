
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

-- | The name of the config file a gopherhole uses...
burrowGopherholeDefaultConfigPath :: FilePath
burrowGopherholeDefaultConfigPath = "data/burrow.toml"

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