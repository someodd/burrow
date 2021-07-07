-- FIXME: use filepath to create good paths 
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Build

import Options.Applicative hiding (ParseError)
import Paths_burrow (version)
import Data.Version (showVersion)

-- | Model for the CLI options
-- usage: burrow.hs source-dir-or-file --version --help --destination=some-dir
-- if out-dir is specified, output to directory and not std-out. must be specified
-- if source is directory. specify destination to not use stdout or if
-- the source is a directory
data Opts = Opts
  { optSpacecookie :: Bool -- Use .gopher
  }

-- FIXME: make it so build is only one of many options/actions. you can also create phlog posts and list tags. Use a phlog post template?
-- TODO/FIXME:
-- Need to instead glob the input directory for *.txt.mustache first, then *.md.mustache
-- in order to create directories and files for gopherspace, respectively.
main :: IO ()
main = do
  opts <- execParser optsParser
  buildGopherhole (optSpacecookie opts)
 where
  optsParser :: ParserInfo Opts
  optsParser =
    info
      (helper <*> versionOption <*> programOptions)
      (fullDesc <> progDesc "burrow " <>
       header "burrow: build gopherholes using Mustache and Markdown")

  versionOption :: Parser (a -> a)
  versionOption = infoOption (showVersion version) (long "version" <> help "Show version")

  programOptions =
    Opts <$>
    switch
      (long "spacecookie" <> help "Parse index.md.mustache files to .gophermap files")
