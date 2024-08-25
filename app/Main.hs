{-# LANGUAGE OverloadedStrings #-}
module Main where

import Build
import Options.Applicative hiding (ParseError)
import Paths_burrow (version)
import Data.Version (showVersion)
import SpacecookieClone.Serve (runServerWithConfig)
import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Config
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.Posix (changeWorkingDirectory)
import System.FilePath (splitPath, joinPath)
import Data.List (isSuffixOf, findIndex)

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs
  { prefShowHelpOnEmpty = True
  }

data SubCommand
  = Build { buildSpacecookie :: Bool, burrowToml :: Maybe FilePath }
  -- ^ Subcommand to build a gopherhole.
  | Serve { burrowToml :: Maybe FilePath, watchChanges :: Bool }
  deriving Show

data MainOptions = MainOptions
  { subcommand :: SubCommand
  } deriving Show

mainCommand :: Parser MainOptions
mainCommand = MainOptions <$> subCommands

versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version) (long "version" <> help "Show version")

subCommands :: Parser SubCommand
subCommands =
  hsubparser
    ( command "build" (info buildSubCommand
        ( progDesc "Create gopherhole according to config" ))
    <> command "serve" (info serveSubCommand
        ( progDesc "Launch spacecookie server according to config" ))
    )

buildSubCommand :: Parser SubCommand
buildSubCommand = Build
  <$> switch
        (long "spacecookie" <> help "Parse index.md.mustache files to .gophermap files")
  <*> optional (strOption
        (long "config" <> metavar "FILE" <>
         help ("Path to burrow configuration file (default: " <> show Config.burrowGopherholeDefaultConfigPath <> ")")))

serveSubCommand :: Parser SubCommand
serveSubCommand = Serve
  <$> optional (strOption
        (long "config" <> metavar "FILE" <>
         help ("Path to burrow configuration file (default: " <> show Config.burrowGopherholeDefaultConfigPath <> ")")))
  <*> switch
        (long "watch" <> help "Watch for changes in burrowsrc and rebuild")

mainParser :: ParserInfo MainOptions
mainParser = info (helper <*> versionOption <*> mainCommand)
      (fullDesc <> progDesc ("burrow v" <> showVersion version) <>
       header "Build elaborate static gopherholes using Mustache and Markdown.")

-- FIXME: i want it to reload the server too on change!
-- FIXME: error where it changes to directory to serve so now it can't build the gopherhole (after build), config
-- may need to be gone over for abs paths or something. also are perms still dropped?!
{- | Serve the gopherhole, if there are any changes in the source path, rebuild the gopherhole.

Will also rebuild the gopherhole based on whatever the state of the configuration file is.

-}
watchServe :: FilePath -> FilePath -> Config.Config -> IO ()
watchServe configFilePath projectRootPath config = do
  putStrLn "Starting the server..."
  directoryPathToWatch <- canonicalizePath $ T.unpack $ Config.sourcePath (Config.general config)

  -- Run the server in a separate thread
  --_ <- forkIO $ runServerWithConfig (Config.spacecookie config)

  putStrLn "Watching for changes in burrowsrc..."
  
  
  -- Watch the directory for changes and rebuild on changes
  withManager $ \mgr -> do
    _ <- watchDir
      mgr
      directoryPathToWatch
      (const True) -- Watch all changes
      (\_ -> do
          putStrLn "Change detected, about to rebuild..."
          changeWorkingDirectory projectRootPath
          currentConfigState <- Config.getConfig configFilePath
          putStrLn "Got current config state, starting build..."
          buildGopherhole currentConfigState True
          putStrLn "Rebuild complete.")
    
    -- Keep the watcher alive
    changeWorkingDirectory projectRootPath
    runServerWithConfig (Config.spacecookie config)
    forever $ threadDelay 1000000

{- | Get the config file from path and also specify the "project root" as
something relative to the config file.

The config file resides in a directory called data/ in the project root.

-}
getConfigSpecial
  :: Maybe FilePath
  -- ^ path to the data/burrow.toml file
  -> IO (Config.Config, FilePath, FilePath)
  -- ^ (the config, path to config, project root) -- absolute paths.
getConfigSpecial maybeConfigPath = do
  absConfigPath <- maybe (return Config.burrowGopherholeDefaultConfigPath) canonicalizePath maybeConfigPath

  case ensureInDataDir absConfigPath of
    Nothing -> error $ "Config file must be in a directory named data/, but got: " ++ absConfigPath
    Just projectRoot -> do
      config <- Config.getConfig absConfigPath
      pure (config, absConfigPath, projectRoot)

-- FIXME
-- Ensure the file is inside a "data/" directory and get the parent directory of "data/"
ensureInDataDir :: FilePath -> Maybe FilePath
ensureInDataDir filePath =
  let pathParts = splitPath filePath
      dataIndex = findIndex ("data/" `isSuffixOf`) pathParts
  in case dataIndex of
       Just idx -> Just (joinPath $ take idx pathParts)
       Nothing -> Nothing

main :: IO ()
main = do
  opts <- customExecParser parserPrefs mainParser
  case subcommand opts of
    Build buildSpacecookieFlag maybeConfigPath -> do
      (config, _, _) <- getConfigSpecial maybeConfigPath
      buildGopherhole config buildSpacecookieFlag
    Serve maybeConfigPath watch -> do
      (config, absConfigPath, projectRoot) <- getConfigSpecial maybeConfigPath
      if watch
        then watchServe absConfigPath projectRoot config
        else runServerWithConfig (Config.spacecookie config)