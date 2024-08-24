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

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs
  { prefShowHelpOnEmpty = True
  }

data SubCommand
  = Build { buildSpacecookie :: Bool, gopherholeIni :: Maybe FilePath }
  -- ^ Subcommand to build a gopherhole.
  | Serve { spacecookieJson :: FilePath, watchChanges :: Bool }
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
        ( progDesc "Create gopherhole according to gopherhole.ini" ))
    <> command "serve" (info serveSubCommand
        ( progDesc "Launch spacecookie server with given configuration" ))
    )

buildSubCommand :: Parser SubCommand
buildSubCommand = Build
  <$> switch
        (long "spacecookie" <> help "Parse index.md.mustache files to .gophermap files")
  <*> optional (strOption
        (long "config" <> metavar "FILE" <>
         help "Path to gopherhole configuration file (default: gopherhole.ini)"))

serveSubCommand :: Parser SubCommand
serveSubCommand = Serve
  <$> strOption
        (long "config" <> metavar "FILE" <>
         help "Path to spacecookie configuration file")
  <*> switch
        (long "watch" <> help "Watch for changes in burrowsrc and rebuild")

mainParser :: ParserInfo MainOptions
mainParser = info (helper <*> versionOption <*> mainCommand)
      (fullDesc <> progDesc ("burrow v" <> showVersion version) <>
       header "Build elaborate static gopherholes using Mustache and Markdown.")

main :: IO ()
main = do
  opts <- customExecParser parserPrefs mainParser
  case subcommand opts of
    -- why isn't spacecookie a config setting?! change that!
    Build buildSpacecookieFlag maybeConfigPath -> do
      buildGopherhole maybeConfigPath buildSpacecookieFlag
    Serve spacecookiePath watch -> do
      putStrLn $ "Serving using config: " ++ spacecookiePath
      if watch
        then do
          putStrLn "Watching for changes in burrowsrc..."
          config <- Config.getConfig spacecookiePath
          let directoryPathToWatch = T.unpack $ Config.sourcePath (Config.general config)
          withManager $ \mgr -> do
            _ <- watchDir
              mgr
              directoryPathToWatch
              (const True)
              (\_ -> do
                  putStrLn "Change detected, rebuilding..."
                  buildGopherhole (Just spacecookiePath) True
                  putStrLn "Rebuild complete.")
            runServerWithConfig (Config.spacecookie config)
            forever $ threadDelay 1000000
        else do
          config <- Config.getConfig spacecookiePath
          runServerWithConfig (Config.spacecookie config)