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

import System.Directory (copyFile)
import qualified Data.HashMap.Strict as H
import Data.List (isSuffixOf)
--import Data.Foldable (traverse_)
import System.FilePath (takeDirectory, takeFileName)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory (createDirectoryIfMissing)
import System.FilePattern.Directory
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Commonmark hiding (addAttribute, escapeURI)
import Text.Mustache
import qualified Text.Mustache.Types as Mtype

import Control.Monad.Reader

import Phlog (renderTagIndexes, renderMainPhlogIndex, FrontMatter, getFrontMatter)
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


-- | File extensions and which Markdown parser will be used for them.
suffixParseMap :: Map.Map ParseSuffix ParseType
suffixParseMap = Map.fromList
  -- FIXME: it didn't have a period to begin with
  [ (textFileSuffix, GopherFileType)
  , (gophermapSuffix, GopherMenuType)
  ]

-- | A relative path to a file to parse (excluding its parent-most directory)
-- and which parser shall be used to parse it.
--
-- See also: `getSourceFiles`.
type SourceFile = (FilePath, ParseType)

-- | Get a list of source files' file paths to be used in order to construct
-- the gopherhole, as well as the type of parser to use to render those paths.
getSourceFiles :: FilePath -> IO [SourceFile]
getSourceFiles sourceDirectory = do
  filesMatching <- getDirectoryFiles sourceDirectory ["**/*"]
  pure $ fmap (\x -> (x, getParseType x)) filesMatching


-- | Get the appropriate ParseType for the supplied file path.
--
-- Burrow relies on the double file extension format of
-- something like *.txt.mustache.
--
-- Suffixes/extensions which are not mapped to a parser shall 
-- be marked as `Skip`.
--
-- >>> getParseType "some/path/foo/bar/filename.text.md.mustache"
-- GopherFileType
-- >>> getParseType "some/path/filename.jpg"
-- Skip
getParseType :: FilePath -> ParseType
getParseType filePath =
  case fmap reverse $ splitOn "." (reverse filePath) of
    maybeMustacheExtension:"md":maybeFileExtension:_ ->
      let parseSuffix = "." ++ maybeFileExtension ++ ".md." ++ maybeMustacheExtension
      in case Map.lookup parseSuffix suffixParseMap of
           Just parseType -> parseType
           Nothing -> Skip
    _ -> Skip


-- | Match the "somepartial" of */*/*.somepartial.partial.*.md.mustache
--
-- Will return Nothing if doesn't use a partial or if the extension isn't
-- mapped to a Markdown parser.
--
-- >>> matchPartial "foo/bar/afile.phlogpost.partial.text.md.mustache"
-- Just ("phlogpost", ".text.md.mustache")
matchPartial :: FilePath -> Maybe (String, String)
matchPartial filePath =
  case getParseType filePath of
    Skip -> Nothing
    _ ->
      let filename = takeFileName filePath
          dotSplit = reverse $ splitOn "." filename
      in case dotSplit of
          -- FIXME: this feels very hardcoded. Should use some of the variables for file extensions
          -- as well as the partialExtension variable.
           "mustache":"md":fileType:"partial":partialName:_ -> Just (partialName, "." ++ fileType ++ ".md.mustache")
           _ -> Nothing


-- TODO: handle frontmatter here?
-- | Create a `Recipe` for rendering a file.
createRenderRecipe :: FilePath -> FilePath -> Bool -> SourceFile -> IO FileRenderRecipe
createRenderRecipe sourceDirectory destinationDirectory spaceCookie (filePath, parseType) =
  case matchPartial filePath of
    Just (templateToUse, extension) -> recipeWithPartial templateToUse extension
    Nothing -> pure recipeWithoutPartial
 where
  -- Create the recipe for a file that does not use partials.
  recipeWithoutPartial :: FileRenderRecipe
  recipeWithoutPartial = 
    FileRenderRecipe
      { pfrSourceDirectory = sourceDirectory
      , pfrDestinationDirectory = destinationDirectory
      , pfrSpacecookie = spaceCookie
      , pfrTemplateToRender = filePathIncludingSourceDirectory
      , pfrParseType = parseType
      , pfrOutPath = filePath
      , pfrIncludePartial = Nothing
      , pfrSubstitutions = dataForMustache
      }

  filePathIncludingSourceDirectory :: String
  filePathIncludingSourceDirectory = sourceDirectory ++ filePath

  -- FIXME: Does not need to be IO, because it doesn't need to load the partial ahead of time... also
  -- shouldn't do newDataFormustache... 
  -- Create the recipe for a file that uses partials.
  recipeWithPartial :: String -> String -> IO FileRenderRecipe
  recipeWithPartial templateToUse extension = do
    let partial'sTemplatePath = "templates/" ++ templateToUse ++ extension
    partial <- readFile filePathIncludingSourceDirectory
    -- FIXME, TODO: it feels like this is being done twice!
    let newDataForMustache = (partialExtension, Mtype.String (T.pack partial)):dataForMustache
    pure $ FileRenderRecipe
             { pfrSourceDirectory = sourceDirectory
             , pfrDestinationDirectory = destinationDirectory
             , pfrSpacecookie = spaceCookie
             , pfrTemplateToRender = filePathIncludingSourceDirectory
             , pfrParseType = parseType
             , pfrOutPath = filePath
             , pfrIncludePartial = Just partial'sTemplatePath
             , pfrSubstitutions = newDataForMustache
             }


-- | FrontMatter meta that has been collected. UNUSED
--type CollectedMeta = [FrontMatter]

-- TODO: docs
-- NOTE: a good place to add hooks?
-- TODO: return index/state? the tag index can be expanded to a more general crawler later to have
-- a hook to do a bunch of stuf with accumulated frontmatter or something?
renderFile :: FilePath -> FilePath -> Bool -> SourceFile -> IO (FilePath, Maybe FrontMatter)
renderFile sourceDirectory destinationDirectory spaceCookie sourceFile@(filePath, parseType) = do
  recipe <- createRenderRecipe sourceDirectory destinationDirectory spaceCookie sourceFile
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
      -- TODO: get and remove frontmatter here
      -- FIXME: parseMustache should NOT load the main file by hand! we should do that
      -- so we can load frontmatter FIRST.
      fileText <- TIO.readFile (sourceDirectory ++ "/" ++ filePath)
      let (frontMatter, restOfDocument) = getFrontMatter fileText
      testContents <- parseMustache restOfDocument recipe :: IO T.Text
      -- FIXME: need a function to parse the frontmatter and remove it from contents instead
      -- right here instead of making up bogus at end
      -- FIXME: this is writing out... that's bad! this function should handle it isntead! so
      -- fix up the markdown function.
      parseMarkdown recipe testContents -- FIXME: needs to just output text instead?
      -- FIXME: output to text instead ^ and then we can also easily handle how to write out?
      -- should have function yeah? this will make chaining manipulations easier?
      case frontMatter of
        Just _ -> pure (fst sourceFile, frontMatter)
        Nothing -> pure (fst sourceFile, Nothing)
 where
  -- Don't parse; just copy the file to the target directory.
  noParseOut :: FileRenderRecipe -> IO ()
  noParseOut recipe = do
    let destination = pfrDestinationDirectory recipe ++ filePath
        destinationDirectory' = takeDirectory destination
        source = pfrSourceDirectory recipe ++ filePath
    createDirectoryIfMissing True destinationDirectory'-- FIXME: is this any different than toplevel destinationDirectory?
    copyFile source destination


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
  }


-- TODO: move to Mustache?
-- TODO: just have a WHERE function for what happens if is partial and one for what if not
-- TODO: the recipe record names are horrible FIXME
-- TODO: this is hard to understand
-- TODO: don't load main file here. you wanna load frontmatter FIRST before using this function and use frontmatter to insert into recipe which should have a Maybe FrontMater part?
-- TODO: prepareTemplate = do
-- | Prepares the file which needs to be parsed as a Mustache template.
parseMustache :: T.Text -> FileRenderRecipe -> IO T.Text
parseMustache mainText recipe = do
  -- TODO: insert global substitutions from frontmatter, also add frontmatter to recipe prior?
  mainTemplate <-
    case pfrIncludePartial recipe of
      Just parentTemplatePath -> newPrepareTemplateUsingParent parentTemplatePath
      Nothing -> prepareTemplate
  -- could put this in the prepareTemplate?
  -- should put this stuff in Mustache
  -- FIXME: what's happening here?!
  let k = Map.fromList (pfrSubstitutions recipe) :: Map.Map T.Text Mtype.Value
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
    let mainTemplateCache = H.fromList [(T.unpack partialExtension, mainTemplate)] :: H.HashMap String Template
    parentTemplate <- compileTemplateWithCache searchSpace mainTemplateCache parentTemplatePath
    case parentTemplate of
      Left err -> error $ show err
      Right template -> pure template
  {-  
  prepareTemplateUsingParent :: FilePath -> IO Template
  prepareTemplateUsingParent parentTemplatePath = do
    --- the "parent template" here is the partial in which we insert the actual/main content into.
    parentTemplate <- getCompiledTemplate searchSpace parentTemplatePath
    -- Now make the main template, which we put inside parent template as the "partial."
    let templateCache = H.insert (T.unpack partialExtension) parentTemplate (partials parentTemplate)
    compiled' <- compileTemplateWithCache searchSpace templateCache (pfrTemplateToRender recipe)
    case compiled' of
      Left err -> error $ show err
      Right template -> pure template
  -}

  -- | Prepare a template normally.
  prepareTemplate :: IO Template
  prepareTemplate = do
    mainTemplate <- getCompiledTemplate searchSpace (pfrTemplateToRender recipe)
    pure mainTemplate


-- FIXME: this does too much, including writing out. should just give back bytes or something idk
-- FIXME: should just output IO T.Text
parseMarkdown :: FileRenderRecipe -> T.Text -> IO ()
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
  -- This is where the file will be writen out to.
  filePath :: FilePath
  filePath = pfrOutPath recipe

  -- When used needs to specify the type. 
  parseCommonmark testContents' = commonmarkWith defaultSyntaxSpec "test" testContents'

  -- Write text to the target/built directory, creating directories in the process if needed.
  -- Will also write out to the file name .gophermap if the input file name matched spacecookieGophermapName.
  writeOut :: T.Text -> IO ()
  writeOut text = do
    let outPath =
          if (pfrSpacecookie recipe) && spacecookieGophermapName `isSuffixOf` filePath
            then let x = (takeDirectory $ pfrDestinationDirectory recipe ++ filePath) in x ++ "/.gophermap"
            else pfrDestinationDirectory recipe ++ filePath
        directory = takeDirectory $ pfrDestinationDirectory recipe ++ (pfrOutPath recipe)
    createDirectoryIfMissing True directory
    writeFile outPath $ T.unpack text

  -- Parse the contents as a text file for gopherspace and write out to the target directory.
  parseOutGopherFile :: IO ()
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
  parseOutGopherMenu :: IO ()
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
  sourceFiles <- getSourceFiles sourceDir :: IO [SourceFile]
  filePathFrontMatter <- traverse (renderFile sourceDir destDir spaceCookie) sourceFiles
  renderTagIndexes filePathFrontMatter
  renderMainPhlogIndex filePathFrontMatter
