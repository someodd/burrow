-- | The configuration file used when building a gopherhole.
--
-- The user can use the configuration file to tweak various settings
-- altering the gopherhole building process.
module Config (getConfig, getConfigValue, ConfigParser) where

import Data.ConfigFile


-- | The name of the config file a gopherhole uses...
burrowGopherholeConfig :: FilePath
burrowGopherholeConfig = "data/gopherhole.ini"

-- | `emptyCP`, but doesn't toLower the options.
customEmptyCP :: ConfigParser
customEmptyCP = emptyCP { optionxform = id }


-- | Get the ConfigParser from file.
getConfig :: IO ConfigParser
getConfig = do
  val <- readfile customEmptyCP burrowGopherholeConfig
  case val of
    Left readError -> error $ show readError
    Right cp -> pure cp


-- | Easy way to read a configuration value from file.
getConfigValue :: ConfigParser -> SectionSpec -> OptionSpec -> IO String
getConfigValue configParser section option =
  let potentialValue = get configParser section option
  in case potentialValue of
    Left readError -> error $ show readError
    Right value -> pure value
