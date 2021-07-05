-- FIXME: maybe use the filepath library to ensure slashes in the right place, for consistency, avoiding terrible bugs
-- FIXME: there needs to be a consistent name for the Burrow partial/template system: parentTemplate.
-- actual file being parsed which uses the parser.
{-# LANGUAGE OverloadedStrings          #-}
{-
 Render the gopherhole to a new directory from a source directory made up of
 Markdown, Mustache, and other special syntax to make complex gopherholes
 easier to manage.

 This module requires a general understanding of the Mustache package and langauge.
 Knowing Commonmark helps, too. I also am using the Jekyll specification of
 "frontmatter."

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
import System.FilePath (takeDirectory, (</>), (<.>), isExtensionOf)

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


-- | Content type that the builder recognizes/finds useful in the build
-- process. This describes the kind of file it will be as an end product in the
-- gopherhole.
data ContentType = GopherFileType
                 -- ^ Parse to plaintext ASCII-art-style file.
                 | GopherMenuType
                 -- ^ Will not be parsed! Will simply be copied.
                 deriving (Show, Eq)


-- | The content type in gopherspace as far as the renderer is concerned. This
-- helps inform the file's build process, like which Markdown parser (if any)
-- to use by default, or if the file should only be copied.
data BuildJobType = RenderEngine ContentType
                  -- ^ Decides which rendering process to go through...
                  -- NOTE: for below: maybe better named "CopyOnly!"
                  | SimplyCopy
                  deriving (Show, Eq)


-- | A relative path to a file to parse (excluding its parent-most directory)
-- and which parser shall be used to parse it.
--
-- See also: `getSourceFiles`.
type SourceFile = (FilePath, BuildJobType)

-- NOTE: when I say "skip" here I mean not using a RenderEngine.
-- | Get a list of source files' file paths to be used in order to construct
-- the gopherhole, as well as the type of parser to use to render those paths.
getSourceFiles :: FilePath -> IO [SourceFile]
getSourceFiles sourceDirectory = do
  config <- getConfig
  dontSkipThese <- fmap words $ getConfigValue config "general" "buildExtensions"
  filesMatching <- getDirectoryFiles sourceDirectory ["**/*"]
  pure $ fmap (\x -> (x, getBuildJobType dontSkipThese x)) filesMatching
 where
  getBuildJobType :: [String] -> FilePath -> BuildJobType
  getBuildJobType dontSkipThese filePath
    | any (`isExtensionOf` filePath) dontSkipMenuExtensions = RenderEngine GopherMenuType
    | any (`isExtensionOf` filePath) dontSkipExtensions = RenderEngine GopherFileType
    | otherwise = SimplyCopy
   where
     dontSkipMenuExtensions = map (".menu" <.>) dontSkipThese
     dontSkipExtensions = map ("." ++) dontSkipThese


-- FIXME: this may result in the file being read twice.
-- | Create a `FileRenderRecipe` for rendering a file.
createRenderRecipe :: FilePath -> FilePath -> Bool -> FilePath -> ContentType -> IO (FileRenderRecipe, Maybe (FrontMatter, T.Text))
createRenderRecipe sourceDirectory destinationDirectory spaceCookie filePath contentType = do
  outPath <- finalFilePath
  let defaultRecipe = FileRenderRecipe
        { frrSourceDirectory = sourceDirectory
        , frrDestinationDirectory = destinationDirectory
        , frrSpacecookie = spaceCookie
        , frrTemplateToRender = sourcePath
        , frrContentType = contentType
        , frrOutPath = outPath
        , frrIncludePartial = Nothing
        , frrSubstitutions = dataForMustache
        , frrSkipMustache = False
        , frrSkipMarkdown = False
        }

  fileText <- TIO.readFile sourcePath
  let (frontMatter, restOfDocument) = getFrontMatter filePath fileText
      templateToUse = frontMatter >>= fmParentTemplate
      renderAs = case frontMatter >>= fmRenderAs of
                   Just renderName -> case renderName of
                                        "menu" -> GopherMenuType
                                        "file" -> GopherFileType
                                        _ -> error "unsupported renderAs" -- FIXME: bad error.
                   Nothing -> contentType
  variablePairs <- traverse toVariablePairs frontMatter
  let recipeFrontMatterChanges = defaultRecipe
        { frrSubstitutions = dataForMustacheWithFrontmatter frontMatter variablePairs
        , frrContentType = renderAs
        , frrSkipMustache = fromMaybe False (frontMatter >>= fmSkipMustache >>= Just :: Maybe Bool)
        , frrSkipMarkdown = fromMaybe False (frontMatter >>= fmSkipMarkdown >>= Just :: Maybe Bool)
        }
      frontMatterReturnPair = (>>= Just . flip (,) restOfDocument)
  case templateToUse of
    Nothing ->
      pure (recipeFrontMatterChanges, frontMatterReturnPair frontMatter)
    Just templateName ->
      let partial'sTemplatePath = "templates" </> templateName
        -- FIXME, TODO: it feels like this is being done twice!
          newDataForMustache = ("partial", Mtype.String restOfDocument):dataForMustacheWithFrontmatter frontMatter variablePairs
          recipe = recipeFrontMatterChanges { frrIncludePartial = Just partial'sTemplatePath, frrSubstitutions = newDataForMustache }
      in pure (recipe, frontMatterReturnPair frontMatter)
 where
  sourcePath :: FilePath
  sourcePath = sourceDirectory </> filePath

  outputPath :: FilePath
  outputPath = destinationDirectory </> filePath

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
    if spaceCookie && indexName `isSuffixOf` filePath
      then let x = (takeDirectory $ outputPath) in pure $ x </> ".gophermap"
      else pure outputPath


-- FIXME: note that "Skip" means to skip the render process and just copy the file.
-- FIXME: rename, buildFile?
buildFile :: FilePath -> FilePath -> Bool -> SourceFile -> IO FileFrontMatter
buildFile sourceDirectory destinationDirectory _ sourceFile@(filePath, SimplyCopy) = do
  -- Don't parse; just copy the file to the target directory.
  let destination = destinationDirectory </> filePath
      destinationDirectory' = takeDirectory destination
      source = sourceDirectory </> filePath
  createDirectoryIfMissing True destinationDirectory'
  copyFile source destination
  pure (fst sourceFile, Nothing)
buildFile sourceDirectory destinationDirectory spaceCookie sourceFile@(filePath, RenderEngine contentType) = do
  (recipe, maybeFrontMatterAndRestOfDoc) <- createRenderRecipe sourceDirectory destinationDirectory spaceCookie filePath contentType
  fileText <- case maybeFrontMatterAndRestOfDoc of
                Nothing -> TIO.readFile (sourceDirectory </> filePath)
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
  , frrContentType :: ContentType
  -- ^ What kind of file in gopherspace are we building for?
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
  case frrContentType recipe of
    -- Parse the markdown text out as a text file to be served in gopherspace.
    GopherFileType -> do
      parseOutGopherFile
    -- Parse the markdown text out as a gophermap/menu.
    GopherMenuType -> do
      parseOutGopherMenu
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
  sourceFiles <- getSourceFiles sourceDir :: IO [SourceFile]
  filePathFrontMatter <- traverse (buildFile sourceDir destDir spaceCookie) sourceFiles
  renderTagIndexes filePathFrontMatter
  renderMainPhlogIndex filePathFrontMatter
