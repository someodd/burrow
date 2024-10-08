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
import System.Posix (changeWorkingDirectory)
import System.FilePath ((</>))
import Control.Exception (SomeException, try)

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

{- | Serve the gopherhole, if there are any changes to the children of the "source path"
then rebuild the gopherhole.

-}
watchServe :: FilePath -> FilePath -> Config.Config -> IO ()
watchServe configFilePath projectRootPath config = do
  putStrLn "Building first..."
  buildGopherhole projectRootPath config True

  putStrLn "Starting the server..."
  let sourceDirectoryPath = projectRootPath </> defaultSourceDirectoryName

  putStrLn $ "Watching for changes in burrowsrc: " ++ sourceDirectoryPath
  
  -- Watch the directory for changes and rebuild on changes
  withManager $ \mgr -> do
    _ <- watchTree
      mgr
      sourceDirectoryPath
      (const True) -- Watch all changes
      (\_ -> do
          putStrLn "Change detected, about to rebuild..."
          --changeWorkingDirectory projectRootPath
          -- FIXME: enable above and fix config below... should have standard...
          (currentConfigState, _, _) <- Config.getConfigSpecial (Just configFilePath) False
          putStrLn "Got current config state, starting build..."
          result <- try (buildGopherhole projectRootPath currentConfigState True) :: IO (Either SomeException ())
          case result of
            Left ex -> putStrLn $ "Rebuild failed with error: " ++ show ex
            Right _ -> putStrLn "Rebuild complete.")
    -- Keep the watcher alive
    changeWorkingDirectory projectRootPath
    runServerWithConfig (Config.spacecookie config)
    forever $ threadDelay 1000000

main :: IO ()
main = do
  opts <- customExecParser parserPrefs mainParser
  case subcommand opts of
    Build buildSpacecookieFlag maybeConfigPath -> do
      (config, _, projectRootPath) <- Config.getConfigSpecial maybeConfigPath False
      buildGopherhole projectRootPath config buildSpacecookieFlag
    Serve maybeConfigPath watch -> do
      (config, absConfigPath, projectRoot) <- Config.getConfigSpecial maybeConfigPath False
      if watch
        then watchServe absConfigPath projectRoot config
        else runServerWithConfig (Config.spacecookie config)