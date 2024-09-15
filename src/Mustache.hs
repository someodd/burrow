-- | Stuff for managing Mustache templates.
{-# LANGUAGE OverloadedStrings          #-}
module Mustache
  ( getCompiledTemplate
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
import System.FilePath ((</>), takeFileName)
import TextUtils.Containers (AsciiBoxConfig, applyContainer)
import qualified Data.Bifunctor

-- FIXME: will get removed
-- | This is where Mustache will look for files, especially for partials.
--searchSpace :: [FilePath]
--searchSpace = ["./templates", "."]


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
automaticCompileText :: FilePath -> T.Text -> IO Template
automaticCompileText projectRoot templateToRenderText = do
  template <- compileTemplate'
  let [t] = template
  pure t
 where
  compileTemplate' = do
    let searchSpace = [projectRoot </> "templates", projectRoot]
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

-- fix me
mustacheContainerize :: MustacheCachedContainers -> T.Text -> T.Text
mustacheContainerize containers text = do
  let (filePath, rest) = T.breakOn " " text
      (spaces, body) = T.breakOn " " (T.drop 1 rest) -- drop the first space
      body' = T.drop 1 body -- drop the second space
      wrapCodeBlock = spaces == "1"
  let box = lookup (T.unpack filePath) (map (Data.Bifunctor.first takeFileName) containers)
  case box of
    Nothing -> error $ "No box found for: " ++ T.unpack filePath ++ show containers
    Just box' ->
      if wrapCodeBlock
        then "```\n" <> (applyContainer box' body') <> "\n```"
        else (applyContainer box' body')

-- | filepath should just be filename
type MustacheCachedContainers = [(FilePath, AsciiBoxConfig)]

-- | Global variables which can be access by a Mustache file being parsed.
--
-- These are the "substitutions" for Mustache. These are functions (lambdas),
-- and regular string values.
--
-- These substitutions are later modified to include a Mustache partial for
-- the Burrow template/partial system.
dataForMustache :: MustacheCachedContainers -> [(T.Text, Mtype.Value)]
dataForMustache containers =
    [ ("justify2", overText justify2)
    , ("justify", overText justify')
    , ("columnate2", overText columnate2)
    , ("containerize", overText (mustacheContainerize containers))
    ]
