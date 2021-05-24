-- FIXME: use filepath to create good paths 
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Build

import Options.Applicative hiding (ParseError)

-- | Model for the CLI options
-- usage: burrow.hs source-dir-or-file --version --help --destination=some-dir
-- if out-dir is specified, output to directory and not std-out. must be specified
-- if source is directory. specify destination to not use stdout or if
-- the source is a directory
data Opts = Opts
  { optSourceDir :: !FilePath -- Should be a FilePath
  , optDestDir :: !FilePath -- Should be a FilePath
  , optSpacecookie :: Bool -- Use .gopher
  }

-- TODO/FIXME:
-- Need to instead glob the input directory for *.txt.mustache first, then *.md.mustache
-- in order to create directories and files for gopherspace, respectively.
main :: IO ()
main = do
  opts <- execParser optsParser
  buildGopherhole (optSourceDir opts) (optDestDir opts) (optSpacecookie opts)
 where
  optsParser :: ParserInfo Opts
  optsParser =
    info
      (helper <*> versionOption <*> programOptions)
      (fullDesc <> progDesc "burrow " <>
       header "burrow: build gopherholes using Mustache and Markdown")

  versionOption :: Parser (a -> a)
  versionOption = infoOption "0.0" (long "version" <> help "Show version")

  programOptions =
    Opts <$> strOption (long "source" <> metavar "DIR" <> help "Directory to parse.") <*>
    strOption
      (long "destination" <> metavar "DIR" <>
       help "The directory to output to (will be created if does not exist).") <*>
    switch
      (long "spacecookie" <> help "Parse index.md.mustache files to .gophermap files")
