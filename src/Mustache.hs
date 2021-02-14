-- | Stuff for managing Mustache templates.
{-# LANGUAGE OverloadedStrings          #-}
module Mustache
  ( searchSpace
  , getCompiledTemplate
  , dataForMustache
  ) where

import qualified Data.Text as T

import Text.Mustache (overText, automaticCompile, Template)
import qualified Text.Mustache.Types as Mtype

import TextUtils


-- | This is where Mustache will look for files, especially for partials.
searchSpace :: [FilePath]
searchSpace = ["./templates", "."]


-- FIXME: what is this even?!
getCompiledTemplate :: [FilePath] -> FilePath -> IO Template
getCompiledTemplate searchSpace' templateToRenderPath = do
  compiled <- automaticCompile searchSpace' templateToRenderPath
  case compiled of
    Left err -> error $ show err
    Right template -> pure template


-- | Global variables which can be access by a Mustache file being parsed.
--
-- These are the "substitutions" for Mustache. These are functions (lambdas),
-- and regular string values.
--
-- These substitutions are later modified to include a Mustache partial for
-- the Burrow template/partial system.
dataForMustache :: [(T.Text, Mtype.Value)]
dataForMustache =
  [ ("title", Mtype.String "My Gopherhole")
  , ("justify2", overText justify2)
  , ("justify", overText justify')
  , ("columnate2", overText columnate2)
  ]
