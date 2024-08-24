-- | The configuration file used when building a gopherhole.
--
-- The user can use the configuration file to tweak various settings
-- altering the gopherhole building process.
module Config (getConfig, getConfigValue, getConfigValuePolymorphic, ConfigParser, burrowGopherholeDefaultConfigPath) where

import Data.ConfigFile


-- | The name of the config file a gopherhole uses...
burrowGopherholeDefaultConfigPath :: FilePath
burrowGopherholeDefaultConfigPath = "data/gopherhole.ini"

-- | `emptyCP`, but doesn't toLower the options.
customEmptyCP :: ConfigParser
customEmptyCP = emptyCP { optionxform = id }


-- | Get the ConfigParser from file.
getConfig :: FilePath -> IO ConfigParser
getConfig configPath = do
  val <- readfile customEmptyCP configPath
  case val of
    Left readError -> error $ show readError
    Right cp -> pure cp

-- TODO:
{- | Load the spacecooke-clone configuration from the INI.

-}


-- | Easy way to read a configuration value from file.
getConfigValue :: ConfigParser -> SectionSpec -> OptionSpec -> IO String
getConfigValue configParser section option =
  let potentialValue = get configParser section option
  in case potentialValue of
    Left readError -> error $ show readError
    Right value -> pure value

-- | Easy way to read a configuration value from file.
getConfigValuePolymorphic :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> IO a
getConfigValuePolymorphic configParser section option =
  let potentialValue = get configParser section option
  in case potentialValue of
    Left readError -> error $ show readError
    Right value -> pure value
