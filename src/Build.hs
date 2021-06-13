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
import System.FilePath (takeDirectory)

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


-- FIXME: this may result in the file being read twice.
-- | Create a `FileRenderRecipe` for rendering a file.
createRenderRecipe :: FilePath -> FilePath -> Bool -> SourceFile -> IO (FileRenderRecipe, Maybe (FrontMatter, T.Text))
createRenderRecipe sourceDirectory destinationDirectory spaceCookie (filePath, parseType) = do
  outPath <- finalFilePath
  let defaultRecipe = FileRenderRecipe
        { frrSourceDirectory = sourceDirectory
        , frrDestinationDirectory = destinationDirectory
        , frrSpacecookie = spaceCookie
        , frrTemplateToRender = sourceDirectory ++ filePath
        , frrParseType = parseType
        , frrOutPath = outPath
        , frrIncludePartial = Nothing
        , frrSubstitutions = dataForMustache
        , frrSkipMustache = False
        , frrSkipMarkdown = False
        }

  if parseType == Skip
    then
      pure (defaultRecipe, Nothing)
    else do
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
      let recipeFrontMatterChanges = defaultRecipe
            { frrSubstitutions = dataForMustacheWithFrontmatter frontMatter variablePairs
            , frrParseType = renderAs
            , frrSkipMustache = fromMaybe False (frontMatter >>= fmSkipMustache >>= Just :: Maybe Bool)
            , frrSkipMarkdown = fromMaybe False (frontMatter >>= fmSkipMarkdown >>= Just :: Maybe Bool)
            }
          frontMatterReturnPair = (>>= Just . flip (,) restOfDocument)
      case templateToUse of
        Nothing ->
          pure (recipeFrontMatterChanges, frontMatterReturnPair frontMatter)
        Just templateName ->
          let partial'sTemplatePath = "templates/" ++ templateName
            -- FIXME, TODO: it feels like this is being done twice!
              newDataForMustache = ("partial", Mtype.String restOfDocument):dataForMustacheWithFrontmatter frontMatter variablePairs
              recipe = recipeFrontMatterChanges { frrIncludePartial = Just partial'sTemplatePath, frrSubstitutions = newDataForMustache }
          in pure (recipe, frontMatterReturnPair frontMatter)
 where
  dataForMustacheWithFrontmatter :: Maybe FrontMatter -> Maybe [(T.Text, T.Text)] -> [(T.Text, Mtype.Value)]
  dataForMustacheWithFrontmatter maybeFrontMatter variablePairs =
    let hardcodedFrontMatterVars = fromMaybe [] (variablePairs >>= Just . map (id . fst &&& Mtype.String . snd)) ++ dataForMustache
        userFrontMatterVars = fromMaybe [] ( maybeFrontMatter >>= \fm -> fmap (Map.toList . Map.map (Mtype.String)) $ fmVariables fm ) :: [(T.Text, Mtype.Value)]
    in hardcodedFrontMatterVars ++ userFrontMatterVars

  -- | Some magic for choosing the target path to write to for the file being
  -- parsed. This is because of the .gopherpath/directory index behavior.
  finalFilePath :: IO FilePath
  finalFilePath = do
    config <- getConfig
    indexName <- getConfigValue config "general" "directoryMapName"
    let outPath =
          if spaceCookie && indexName `isSuffixOf` filePath
            then let x = (takeDirectory $ destinationDirectory ++ filePath) in x ++ "/.gophermap"
            else destinationDirectory ++ filePath
    pure outPath


renderFile :: FilePath -> FilePath -> Bool -> SourceFile -> IO FileFrontMatter
renderFile sourceDirectory destinationDirectory spaceCookie sourceFile@(filePath, parseType) = do
  (recipe, maybeFrontMatterAndRestOfDoc) <- createRenderRecipe sourceDirectory destinationDirectory spaceCookie sourceFile
  if parseType == Skip
    then do
      _ <- noParseOut recipe
      pure (fst sourceFile, Nothing)
    else do
      fileText <- case maybeFrontMatterAndRestOfDoc of
                    Nothing -> TIO.readFile (sourceDirectory ++ "/" ++ filePath)
                    Just (_, restOfDocument) -> pure restOfDocument
      let frontMatter = maybeFrontMatterAndRestOfDoc >>= Just . fst

      testContents <- if frrSkipMustache recipe
        then pure fileText
        else parseMustache fileText recipe :: IO T.Text

      finalContents <- if frrSkipMarkdown recipe
        then pure testContents
        else parseMarkdown recipe testContents :: IO T.Text

      let filePathToWriteTo = frrOutPath recipe
      createDirectoryIfMissing True (takeDirectory filePathToWriteTo)
      writeFile filePathToWriteTo (T.unpack finalContents) -- first time it's written
      pure (fst sourceFile, frontMatter)
 where
  -- Don't parse; just copy the file to the target directory.
  noParseOut :: FileRenderRecipe -> IO ()
  noParseOut recipe = do
    let destination = frrDestinationDirectory recipe ++ filePath
        destinationDirectory' = takeDirectory destination
        source = frrSourceDirectory recipe ++ filePath
    createDirectoryIfMissing True destinationDirectory'
    copyFile source destination


-- | All the settings for a function (`writeOutBasedOn`) to parse a file
-- using Commonmark and Mustache.
data FileRenderRecipe = FileRenderRecipe
  { frrSourceDirectory :: FilePath
  -- ^ The directory which the main file is from.
  , frrDestinationDirectory :: FilePath
  -- ^ The directory where the file will be going.
  , frrSpacecookie :: Bool
  -- ^ If True, turn on the spacecookie behavior.
  , frrTemplateToRender :: FilePath
  -- ^ This is the path to the main file to parse, unless a partial is specified (frrIncludePartial). In the case
  -- a partial is specified, this is the path to the template/ partial file.
  , frrParseType :: ParseType
  -- ^ Which parser to use.
  , frrOutPath :: FilePath
  -- ^ Where the parsed file shall be written out to.
  , frrIncludePartial :: Maybe FilePath
  -- ^ If a partial is being used, the main file to be parsed is instead specified here,
  -- so it may be loaded as a partial named "partial" in the template/.
  , frrSubstitutions :: [(T.Text, Mtype.Value)]
  -- ^ The substitutions to be used when parsing the mustache template. You can think of these
  -- as globals or putting things into scope. But it all needs to be a commonmark value: a
  -- string, a function, a partial.
  , frrSkipMustache :: Bool
  -- ^ If True will skip the Mustache rendering process.
  , frrSkipMarkdown :: Bool
  -- ^ If True will skip the Markdown rendering process.
  } deriving (Show)


-- | Prepares the file which needs to be parsed as a Mustache template, 
-- according to a `FileRenderRecipe`.
--
-- The source of this function is a bit confusing due to the fact that if we
-- are using a parent template then we are using the main file as a partial
-- inserted as a substitution for Mustache named "partial," where the parent
-- template is the main that gets rendered.
parseMustache :: T.Text -> FileRenderRecipe -> IO T.Text
parseMustache mainText recipe = do
  -- The main template will be the main file in question which the recipe is
  -- for if there's no use of parent template, but if the parent template is
  -- used then the mainTemplate will be the parent template, with the file the
  -- recipe is for being inserted as a Mustache substitution named "partial,"
  -- which the main template can call as "partial."
  mainTemplate <-
    case frrIncludePartial recipe of
      Just parentTemplatePath -> newPrepareTemplateUsingParent parentTemplatePath
      Nothing -> prepareTemplate
  pure $ substitute mainTemplate $ (Map.fromList $ frrSubstitutions recipe :: Map.Map T.Text Mtype.Value)
 where
  -- | Prepare a template which will insert itself inside a parent template.
  --
  -- Performs a substitution operation for "partial" (in order to put the file
  -- the recipe is for inside of the specified parent template).
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


-- | Needs IO mainly for the font files. Could be made IO-free if fonts were loaded prior.
parseMarkdown :: FileRenderRecipe -> T.Text -> IO T.Text
parseMarkdown recipe contents =
  case frrParseType recipe of
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
        pure out'

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
        pure out'


-- | The exposed function to parse and copy a directory's files out to a new directory which
-- can be served via the Gopher protocol.
buildGopherhole :: FilePath -> FilePath -> Bool -> IO ()
buildGopherhole sourceDir destDir spaceCookie = do
  --- FIXME: getSourceFiles needs to use the extensio directive in the ini file!
  sourceFiles <- getSourceFiles sourceDir :: IO [SourceFile]
  filePathFrontMatter <- traverse (renderFile sourceDir destDir spaceCookie) sourceFiles
  renderTagIndexes filePathFrontMatter
  renderMainPhlogIndex filePathFrontMatter
