-- | Stuff for managing Mustache templates.
{-# LANGUAGE OverloadedStrings          #-}
module Mustache
  ( searchSpace
  , getCompiledTemplate
  , dataForMustache
  , automaticCompileText
  ) where

import qualified Data.Text as T

import qualified Data.HashMap.Strict as H
import Text.Mustache.Compile
import Text.Mustache
import Text.Mustache.Parser
--import Text.Mustache (compileTemplate, overText, automaticCompile, Template)
import qualified Text.Mustache.Types as Mtype

import TextUtils


-- | This is where Mustache will look for files, especially for partials.
searchSpace :: [FilePath]
searchSpace = ["./templates", "."]


-- | This is like `automaticCompile`, except we use `Text`.
--
-- Based of the documentation in Text.Mustache.Compile:
--
--  "The same can be done manually using getFile, mustacheParser and
--  getPartials."
--    { name     :: String
--  , ast      :: STree
--  , partials :: TemplateCache
--
--  Used for the main document parsing, I guess?
automaticCompileText :: T.Text -> IO Template
automaticCompileText templateToRenderText = do
  template <- compileTemplate'
  let [t] = template
  pure t
 where
  compileTemplate' = do
    let mainPartialAST =
          case parse "partial" templateToRenderText of
            Left err -> error $ show err
            Right ast' -> ast'
        partials' = getPartials mainPartialAST
    partialTemplates <- traverse (getCompiledTemplate searchSpace) partials'
    let templateCache = H.fromList (zip partials' partialTemplates)
    pure $ fmap (flip (Template "partial") templateCache) [mainPartialAST]


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
  [ ("justify2", overText justify2)
  , ("justify", overText justify')
  , ("columnate2", overText columnate2)
  ]
