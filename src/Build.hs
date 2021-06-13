-- FIXME/TODO: this could be organized in a more obvious and simple manner...
-- FIXME: maybe use the filepath library to ensure slashes in the right place, for consistency, avoiding terrible bugs
-- FIXME: there needs to be a consistent name for the Burrow partial/template system.
-- FIXME: make note that all parseable files must be some kind of *.mustache?
-- TODO: note that the partial template used must match the type/extension of the
-- actual file being parsed which uses the parser.
{-# LANGUAGE OverloadedStrings          #-}
{-
 Render the gopherhole to a new directory from a source directory made up of
 Markdown, Mustache, and other special syntax to make complex gopherholes
 easier to manage.

 This module requires a general understanding of the Mustache package and langauge.
 Knowing Commonmark helps, too. I also am copying the Jekyll implementation of
 "frontmatter" in order to implement tags for the time being.

 Terms:

   * Source file: a file from the source directory, which will be used to build
     the gopherhole.
   * Parser: something that can convert a source file to a new output format.
   * Render: to parse a file a source file to its final form to be used in the
     gopherhole.
-}
module Build 
  ( buildGopherhole
  ) 
where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import System.Directory (copyFile)
import qualified Data.HashMap.Strict as H
import Data.List (isSuffixOf)
--import Data.Foldable (traverse_)
import System.FilePath (splitExtension, takeDirectory)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory (createDirectoryIfMissing)
import System.FilePattern.Directory
import qualified Data.Map as Map
import Commonmark hiding (addAttribute, escapeURI)
import Text.Mustache
import qualified Text.Mustache.Types as Mtype

import Control.Monad.Reader

import Config
import FrontMatter (toVariablePairs)
import Phlog (renderTagIndexes, renderMainPhlogIndex, FrontMatter(..), getFrontMatter, FileFrontMatter)
import TextUtils.Headings
import Markdown
import Mustache
import Common


-- | The Markdown parser to use to transform Markdown files to various outputs
-- (like menus/gophermaps) suitable for gopherspace.
data ParseType = GopherFileType
               -- ^ Parse to plaintext ASCII-art-style file.
               | GopherMenuType
               -- ^ Parse to a gophermap/Gopher menu.
               -- NOTE: for below: maybe better named "CopyOnly!"
               | Skip
               -- ^ Will not be parsed! Will simply be copied.
               deriving (Show, Eq)


-- | A relative path to a file to parse (excluding its parent-most directory)
-- and which parser shall be used to parse it.
--
-- See also: `getSourceFiles`.
type SourceFile = (FilePath, ParseType)

-- FIXME: mayeb this isn't how we want to do things since we have frontmatter
-- | Get a list of source files' file paths to be used in order to construct
-- the gopherhole, as well as the type of parser to use to render those paths.
getSourceFiles :: FilePath -> IO [SourceFile]
getSourceFiles sourceDirectory = do
  config <- getConfig
  dontSkipThese <- fmap words $ getConfigValue config "general" "buildExtensions"
  filesMatching <- getDirectoryFiles sourceDirectory ["**/*"]
  pure $ fmap (\x -> (x, getParseType dontSkipThese x)) filesMatching
 where
  getParseType :: [String] -> FilePath -> ParseType
  getParseType dontSkipThese filePath
    | any (`isSuffixOf` filePath) dontSkipMenuExtensions = GopherMenuType
    | any (`isSuffixOf` filePath) dontSkipExtensions = GopherFileType
    | otherwise = Skip
   where
     dontSkipMenuExtensions = map (".menu." ++) dontSkipThese
     dontSkipExtensions = map ("." ++) dontSkipThese


-- FIXME: partials work right, nonpartials dont
-- FIXME: giant mess! drop file extension idea altogether
-- FIXME: take an argument here to decide if you're going to use template or
-- not based off frontmatter spec? loosely coupled tho just send a bool. don't use frontmatter if don't nee dmost.
-- TODO: handle frontmatter here?
-- | Create a `Recipe` for rendering a file.
createRenderRecipe :: FilePath -> FilePath -> Bool -> SourceFile -> IO (FileRenderRecipe, Maybe (FrontMatter, T.Text))
createRenderRecipe sourceDirectory destinationDirectory spaceCookie (filePath, parseType) = do
  -- If the parse type isn't skip then we want...
  if parseType == Skip
    then pure (recipeWithoutPartial Nothing Nothing Skip, Nothing)
    else do
      -- FIXME: the buildExtensions part should be for the getparsetype thingy
      -- and then we still need to define which is menu and which plain?
      -- maybe don't use file extensions at all to specify partials an drender types.
      --config <- getConfig
      --fileExtensions <- fmap words $ getConfigValue "general" "buildExtensions"
      fileText <- TIO.readFile (sourceDirectory ++ "/" ++ filePath)
      let (frontMatter, restOfDocument) = getFrontMatter filePath fileText
          templateToUse = frontMatter >>= fmParentTemplate
          renderAs = case frontMatter >>= fmRenderAs of
                       Just renderName -> case renderName of
                                            "menu" -> GopherMenuType
                                            "file" -> GopherFileType
                                            _ -> error "unsupported renderAs" -- FIXME: bad error.
                       Nothing -> parseType
      variablePairs <- traverse toVariablePairs frontMatter
      case templateToUse of
        Nothing ->
          pure (recipeWithoutPartial variablePairs frontMatter renderAs, frontMatter >>= \x -> Just (x, restOfDocument))
        Just templateName ->
          let (noExtension, extension) = splitExtension templateName
          in pure (recipeWithPartial variablePairs frontMatter fileText noExtension extension renderAs, frontMatter >>= \x -> Just (x, restOfDocument))
 where
  -- THIS ALL BECAME A MESS AGAIN.. the passing of frontmatter and variable pairs...
  defaultRecipe :: Maybe [(T.Text, T.Text)] -> Maybe FrontMatter -> FileRenderRecipe
  defaultRecipe variablePairs frontMatter = FileRenderRecipe
    { pfrSourceDirectory = sourceDirectory
    , pfrDestinationDirectory = destinationDirectory
    , pfrSpacecookie = spaceCookie
    , pfrTemplateToRender = filePathIncludingSourceDirectory
    , pfrParseType = parseType
    , pfrOutPath = filePath
    , pfrIncludePartial = Nothing
    , pfrSubstitutions = dataForMustacheWithFrontmatter variablePairs
    , pfrSkipMustache = fromMaybe False (frontMatter >>= fmSkipMustache >>= Just :: Maybe Bool)
    , pfrSkipMarkdown = fromMaybe False (frontMatter >>= fmSkipMarkdown >>= Just :: Maybe Bool)
    }

  dataForMustacheWithFrontmatter :: Maybe [(T.Text, T.Text)] -> [(T.Text, Mtype.Value)]
  dataForMustacheWithFrontmatter variablePairs =
    fromMaybe [] (variablePairs >>= Just . map (id . fst &&& Mtype.String . snd)) ++ dataForMustache

  -- FIXME: this is all unnecessarily messy!
  -- FIXME: rendertype vs parsetype? what?
  -- Create the recipe for a file that does not use partials.
  recipeWithoutPartial :: Maybe [(T.Text, T.Text)] -> Maybe FrontMatter -> ParseType -> FileRenderRecipe
  recipeWithoutPartial variablePairs frontMatter parseType' = (defaultRecipe variablePairs frontMatter) { pfrParseType = parseType' }

  filePathIncludingSourceDirectory :: String
  filePathIncludingSourceDirectory = sourceDirectory ++ filePath

  -- FIXME: this is confusing.
  -- FIXME: Does not need to be IO, because it doesn't need to load the partial ahead of time... also
  -- shouldn't do newDataFormustache... 
  -- Create the recipe for a file that uses partials.
  recipeWithPartial :: Maybe [(T.Text, T.Text)] -> Maybe FrontMatter -> T.Text -> String -> String -> ParseType -> FileRenderRecipe
  recipeWithPartial variablePairs frontMatter mainTextContents templateToUse extension parseType' =
    let partial'sTemplatePath = "templates/" ++ templateToUse ++ extension
      -- FIXME, TODO: it feels like this is being done twice!
      -- FIXME: hardcoding again
        newDataForMustache = ("partial", Mtype.String mainTextContents):dataForMustacheWithFrontmatter variablePairs
    in (defaultRecipe variablePairs frontMatter)
         { pfrParseType = parseType'
         , pfrIncludePartial = Just partial'sTemplatePath
         , pfrSubstitutions = newDataForMustache
         }


-- | FrontMatter meta that has been collected. UNUSED
--type CollectedMeta = [FrontMatter]

-- TODO: docs
-- NOTE: a good place to add hooks?
-- TODO: return index/state? the tag index can be expanded to a more general crawler later to have
-- a hook to do a bunch of stuf with accumulated frontmatter or something?
renderFile :: FilePath -> FilePath -> Bool -> SourceFile -> IO FileFrontMatter
renderFile sourceDirectory destinationDirectory spaceCookie sourceFile@(filePath, parseType) = do
  (recipe, maybeFrontMatterAndRestOfDoc) <- createRenderRecipe sourceDirectory destinationDirectory spaceCookie sourceFile
  --_ <- error . show $ maybeFrontMatterAndRestOfDoc
  --_ <- error . show $ parseType
  -- TODO/FIXME: you have to implement frontmatter parsing here and pass off the modified
  -- contents, I guess! Would have to use a State I guess to write to for updating and
  -- maintaining an index to finally write out later...
  --
  -- You could just append to an index file!
  -- FIXME: this is broken, because mustache stuff honestly should just be a subset function
  -- of parseMarkdown. Because we only run Mustache on markdown files! This will break
  -- if the input file is something other than a source file which can be located in
  -- the getParseType map.
  if parseType == Skip
    then do
      _ <- noParseOut recipe
      pure (fst sourceFile, Nothing)
    else do
      fileText <- case maybeFrontMatterAndRestOfDoc of
                    Nothing -> TIO.readFile (sourceDirectory ++ "/" ++ filePath)
                    Just (_, restOfDocument) -> pure restOfDocument
      let frontMatter = maybeFrontMatterAndRestOfDoc >>= Just . fst

      -- FIXME use pfr isntead
      -- FIXME: all this skipping gives me a headache to debug
      -- could actually just chain things together (renderers) based on what's available as True
      testContents <- if pfrSkipMustache recipe
        then pure fileText
        else parseMustache fileText recipe frontMatter :: IO T.Text

      finalContents <- if pfrSkipMarkdown recipe
        then pure testContents
        else parseMarkdown recipe testContents :: IO T.Text

      let filePathToWriteTo = pfrOutPath recipe
      finalFilePathToWriteTo <- finalFilePath filePathToWriteTo recipe
      writeFile finalFilePathToWriteTo (T.unpack finalContents) -- first time it's written
      pure (fst sourceFile, frontMatter)
 where
  -- | Some magic for choosing the target path to write to for files being
  -- parsed. This is because of the .gopherpath/directory index behavior.
  finalFilePath :: FilePath -> FileRenderRecipe -> IO FilePath
  finalFilePath filePath' recipe = do
    let outPath =
          if (pfrSpacecookie recipe) && spacecookieGophermapName `isSuffixOf` filePath'
            then let x = (takeDirectory $ pfrDestinationDirectory recipe ++ filePath') in x ++ "/.gophermap"
            else pfrDestinationDirectory recipe ++ filePath'
        directory = takeDirectory $ pfrDestinationDirectory recipe ++ (pfrOutPath recipe)
    createDirectoryIfMissing True directory
    pure outPath

  -- Don't parse; just copy the file to the target directory.
  noParseOut :: FileRenderRecipe -> IO ()
  noParseOut recipe = do
    let destination = pfrDestinationDirectory recipe ++ filePath
        destinationDirectory' = takeDirectory destination
        source = pfrSourceDirectory recipe ++ filePath
    createDirectoryIfMissing True destinationDirectory'-- FIXME: is this any different than toplevel destinationDirectory?
    copyFile source destination


-- FIXME: should include "skip mustache" and "skip markdown" directives
-- FIXME: rename all these pfr things to frr or just recipe as the prefix
-- | All the settings for a function (`writeOutBasedOn`) to parse a file
-- using Commonmark and Mustache.
data FileRenderRecipe = FileRenderRecipe
  { pfrSourceDirectory :: FilePath
  -- ^ The directory which the main file is from.
  , pfrDestinationDirectory :: FilePath
  -- ^ The directory where the file will be going.
  , pfrSpacecookie :: Bool
  -- ^ If True, turn on the spacecookie behavior.
  , pfrTemplateToRender :: FilePath
  -- ^ This is the path to the main file to parse, unless a partial is specified (pfrIncludePartial). In the case
  -- a partial is specified, this is the path to the template/ partial file.
  , pfrParseType :: ParseType
  -- ^ Which parser to use.
  , pfrOutPath :: FilePath
  -- ^ Where the parsed file shall be written out to.
  , pfrIncludePartial :: Maybe FilePath
  -- ^ If a partial is being used, the main file to be parsed is instead specified here,
  -- so it may be loaded as a partial named "partial" in the template/.
  , pfrSubstitutions :: [(T.Text, Mtype.Value)]
  -- ^ The substitutions to be used when parsing the mustache template. You can think of these
  -- as globals or putting things into scope. But it all needs to be a commonmark value: a
  -- string, a function, a partial.
  , pfrSkipMustache :: Bool
  -- ^ If True will skip the Mustache rendering process.
  , pfrSkipMarkdown :: Bool
  -- ^ If True will skip the Markdown rendering process.
  } deriving (Show)


-- FIXME: clean this up
-- FIXME: mainText kinda being ignored?
-- TODO: move to Mustache?
-- TODO: just have a WHERE function for what happens if is partial and one for what if not
-- TODO: the recipe record names are horrible FIXME
-- TODO: this is hard to understand
-- TODO: don't load main file here. you wanna load frontmatter FIRST before using this function and use frontmatter to insert into recipe which should have a Maybe FrontMater part?
-- TODO: prepareTemplate = do
-- | Prepares the file which needs to be parsed as a Mustache template.
parseMustache :: T.Text -> FileRenderRecipe -> Maybe FrontMatter -> IO T.Text
parseMustache mainText recipe maybeFrontMatter = do
  -- TODO: insert global substitutions from frontmatter, also add frontmatter to recipe prior?
  mainTemplate <-
    case pfrIncludePartial recipe of
      Just parentTemplatePath -> newPrepareTemplateUsingParent parentTemplatePath
      Nothing -> prepareTemplate
  -- could put this in the prepareTemplate?
  -- should put this stuff in Mustache
  -- FIXME: what's happening here?!
  let fmSubs = fromMaybe [] ( maybeFrontMatter >>= \fm -> fmap (Map.toList . Map.map (Mtype.String)) $ fmVariables fm ) :: [(T.Text, Mtype.Value)]
  let k = Map.fromList (pfrSubstitutions recipe ++ fmSubs) :: Map.Map T.Text Mtype.Value
      -- where is this substitute function coming from?! is it commonmark to preform the partial substitution?
      testContents = substitute mainTemplate k
      -- also i wish i could do the partial subs... shouldn't that be easy, actually?
  pure testContents
 -- FIXME: can use substitute to replace partials!
 where
  -- the way you use `partials` here is interesting (the function) this may aid in using the text
  -- parser version TODO FIXME
  -- | Prepare a template which will insert itself inside a parent template. Lots of partia magic.
  newPrepareTemplateUsingParent :: FilePath -> IO Template
  newPrepareTemplateUsingParent parentTemplatePath = do
    -- we are going to put mainTemplate in the template cache as a partial as "partial" for the parent template!
    mainTemplate <- automaticCompileText mainText
    let mainTemplateCache = H.fromList [("partial", mainTemplate)] :: H.HashMap String Template
    parentTemplate <- compileTemplateWithCache searchSpace mainTemplateCache parentTemplatePath
    case parentTemplate of
      Left err -> error $ show err
      Right template -> pure template

  -- | Prepare a template normally.
  prepareTemplate :: IO Template
  prepareTemplate = do
    mainTemplate <- automaticCompileText mainText
    pure mainTemplate
    --mainTemplate <- getCompiledTemplate searchSpace (pfrTemplateToRender recipe)
    --pure mainTemplate


-- | Needs IO mainly for the font files. Could be made IO-free if fonts were loaded prior.
parseMarkdown :: FileRenderRecipe -> T.Text -> IO T.Text
parseMarkdown recipe contents =
  case pfrParseType recipe of
    -- NOTE: The way this type is setup seems redundant/adds extra maintenence just use a sumtype like ParseType = GopherFile | GopherMenu? FIXME/NOTE/TODO
    -- Parse the markdown text out as a text file to be served in gopherspace.
    GopherFileType -> do
      parseOutGopherFile
    -- Parse the markdown text out as a gophermap/menu.
    GopherMenuType -> do
      parseOutGopherMenu
    -- Do not parse this file. Simply copy the file to the new destination!
    Skip -> do
      error "parseMarkdown cannot be used on file types not intended for it."
 where
  -- When used needs to specify the type. 
  parseCommonmark testContents' = commonmarkWith defaultSyntaxSpec "test" testContents'

  -- Write text to the target/built directory, creating directories in the process if needed.
  -- Will also write out to the file name .gophermap if the input file name matched spacecookieGophermapName.
  writeOut :: T.Text -> IO T.Text
  writeOut text = do
     pure text

  -- Parse the contents as a text file for gopherspace and write out to the target directory.
  parseOutGopherFile :: IO T.Text
  parseOutGopherFile = do
    out <- parseCommonmark contents :: IO (Either ParseError (ParseEnv GopherFile))
    case out of
      Left parseError -> error $ show parseError
      Right penv -> do
        allTheAsciiFonts <- getAsciiFonts
        let env = Environment { envFonts = allTheAsciiFonts, envInlineOverrides = blankInlineOverrides }
        let (GopherFile out') = runReader penv env
        writeOut out'

  -- Parse the contents as a Gopher menu/gophermap for gopherspace and write out to the target directory.
  parseOutGopherMenu :: IO T.Text
  parseOutGopherMenu = do
    out <- parseCommonmark contents :: IO (Either ParseError (ParseEnv GopherFile))
    case out of
      Left parseError -> error $ show parseError
      Right penv -> do
        allTheAsciiFonts <- getAsciiFonts
        -- FIXME: i'm using "parseLinkToGopherFileLink" in the parseOutGopherMenu thingy...
        let inlineOverrides = InlineOverrides { overrideLink = Just createGopherMenuLink }
        let env = Environment { envFonts = allTheAsciiFonts, envInlineOverrides = inlineOverrides }
        let (GopherFile out') = runReader penv env
        writeOut out'


-- | The exposed function to parse and copy a directory's files out to a new directory which
-- can be served via the Gopher protocol.
buildGopherhole :: FilePath -> FilePath -> Bool -> IO ()
buildGopherhole sourceDir destDir spaceCookie = do
  --- FIXME: getSourceFiles needs to use the extensio directive in the ini file!
  sourceFiles <- getSourceFiles sourceDir :: IO [SourceFile]
  filePathFrontMatter <- traverse (renderFile sourceDir destDir spaceCookie) sourceFiles
  renderTagIndexes filePathFrontMatter
  renderMainPhlogIndex filePathFrontMatter
