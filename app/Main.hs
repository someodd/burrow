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
data BuildOptions = BuildOptions
  { buildSpacecookie :: Bool -- Use .gopher
  }

data Command
  = Build BuildOptions


data MainInterface = { mainCommand :: Command }

-- FIXME: make it so build is only one of many options/actions. you can also create phlog posts and list tags. Use a phlog post template?
-- TODO/FIXME:
-- Need to instead glob the input directory for *.txt.mustache first, then *.md.mustache
-- in order to create directories and files for gopherspace, respectively.
main :: IO ()
main = do
  opts <- execParser parser
  case opts of
    Build buildOptions -> buildGopherhole (buildSpacecookie buildOptions)
 where
  parser :: ParserInfo MainInterface
  parser =
    info
      (helper <*> versionOption <*> (Build <$> buildParser))
      (fullDesc <>        header "burrow: build gopherholes using Mustache and Markdown")

  versionOption :: Parser (a -> a)
  versionOption = infoOption (showVersion version) (long "version" <> help "Show version")

  buildParser =
    hsubparser (command "build" (info buildOptions (progDesc "Build a gopherhole according to gopherhole.ini")))
   where
    buildOptions =
      BuildOptions <$>
      switch
        (long "spacecookie" <> help "Parse index.md.mustache files to .gophermap files")
