{-# LANGUAGE OverloadedStrings #-}
module Main where

import Build
-- Assuming there's a module to handle the serving functionality
-- import Serve

import Options.Applicative hiding (ParseError)
import Paths_burrow (version)
import Data.Version (showVersion)
import SpacecookieClone.Serve (runServer)

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs
  { prefShowHelpOnEmpty = True
  }

data SubCommand
  = Build { buildSpacecookie :: Bool }
  | Serve { configFile :: FilePath } -- Added Serve constructor
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
buildSubCommand = Build <$>
  switch
    (long "spacecookie" <> help "Parse index.md.mustache files to .gophermap files")

serveSubCommand :: Parser SubCommand
serveSubCommand = Serve <$>
  strOption
    (long "config" <> metavar "FILE" <>
     help "Path to spacecookie configuration file")

mainParser :: ParserInfo MainOptions
mainParser = info (helper <*> versionOption <*> mainCommand)
      (fullDesc <> progDesc ("burrow v" <> showVersion version) <>
       header "Build elaborate static gopherholes using Mustache and Markdown.")

main :: IO ()
main = do
  opts <- customExecParser parserPrefs mainParser
  case subcommand opts of
    Build buildOptions -> buildGopherhole buildOptions
    Serve serveConfigFile -> do
      putStrLn $ "Serving using config: " ++ serveConfigFile
      runServer serveConfigFile