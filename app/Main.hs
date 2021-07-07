-- FIXME: use filepath to create good paths 
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Build

import Options.Applicative hiding (ParseError)
import Paths_burrow (version)
import Data.Version (showVersion)

-- bad practice (should have options be a separate type
data SubCommand
  = Build { buildSpacecookie :: Bool }
  deriving Show

-- | The main CLI interface for the main CLI parser.
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
    )

buildSubCommand :: Parser SubCommand
buildSubCommand = Build <$>
  ( switch
      (long "spacecookie" <> help "Parse index.md.mustache files to .gophermap files")
  )

mainParser :: ParserInfo MainOptions
mainParser = info (helper <*> versionOption <*> mainCommand)
      (fullDesc <> progDesc ("burrow v" <> (showVersion version)) <>
       header "Build elaborate static gopherholes using Mustache and Markdown.")

-- FIXME: show help by default if no action specified
-- FIXME: make it so build is only one of many options/actions. you can also create phlog posts and list tags. Use a phlog post template?
main :: IO ()
main = do
  opts <- execParser mainParser
  case subcommand opts of
    Build buildOptions -> buildGopherhole (buildOptions)
