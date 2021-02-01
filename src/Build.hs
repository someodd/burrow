-- FIXME: maybe use the filepath library to ensure slashes in the right place, for consistency, avoiding terrible bugs
-- FIXME: there needs to be a consistent name for the Burrow partial/template system.
-- FIXME: make note that all parseable files must be some kind of *.mustache?
-- TODO: note that the partial template used must match the type/extension of the
-- actual file being parsed which uses the parser.
{-# LANGUAGE OverloadedStrings          #-}
-- | Parse all the files from a directory and write them out to the specified
-- directory where built files go (the files for the gopherhole!).
--
-- Understanding this module may require a general understanding of the Mustache package and language.
-- Knowing the Commonmark package helps, too.
module Build 
  ( doTheParsing
  -- ^ Build the gopherhole!
  ) 
where

import System.Directory (copyFile)
import Data.HashMap.Strict as H
import           Data.Text (unpack, pack, Text, toUpper)
import Data.List (isSuffixOf)
import Data.Foldable (traverse_)
import System.FilePath (takeDirectory, takeFileName, FilePath)

import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePattern.Directory
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Commonmark hiding (addAttribute, escapeURI)
import           Commonmark.Types
import           Commonmark.Entity (lookupEntity)
import Text.Mustache
import qualified Text.Mustache.Types as Mtype

import Control.Monad.Reader
import qualified Data.Text as Text

import TextUtils
import TextUtils.Headings
import Markdown
import Mustache
import Types


-- | This is the special name for generating spacecookie indexes if enabled (gophermaps).
spacecookieGophermapName = "index.md.mustache"


-- NOTE: is there something like this in System.FilePath?
type FileName = FilePath


-- TODO: wouldn't this be better as a sum type?
-- | The Markdown parser to use to transform Markdown files to various outputs
-- (like menus/gophermaps) suitable for gopherspace.
data ParseType = GopherFileType
               -- ^ Parse to plaintext ASCII-art-style file.
               | GopherMenuType
               -- ^ Parse to a gophermap/Gopher menu.
               -- NOTE: for below: maybe better named "CopyOnly!"
               | Skip
               -- ^ Will not be parsed! Will simply be copied.


-- | A file extension which maps to one of Burrow's Markdown parsers.
type ParseSuffix = String


-- FIXME: These extensions don't make sense. It's all Markdown, so it
-- should parse .menu.md.mustache and .txt.md.mustache or something. Where the
-- first extension specifies the output format, the second is the format it's in
-- (Markdown),
-- the last is that it will be assembled (substitutions) with Mustache.
-- | File extensions and which Markdown parser will be used for them.
suffixParseMap :: Map.Map ParseSuffix ParseType
suffixParseMap = Map.fromList
  [ ("txt.mustache", GopherFileType)
  , ("md.mustache", GopherMenuType)
  ]


-- | Get the appropriate ParseType for the supplied file path.
--
-- Burrow relies on the double file extension format of
-- something like *.txt.mustache.
--
-- Suffixes/extensions which are not mapped to a parser shall 
-- be marked as Skip.
--
-- >>> getParseType "some/path/foo/bar/filename.txt.mustache"
-- GopherFileType
-- >>> getParseType "some/path/filename.jpg"
-- Skip
getParseType :: FilePath -> ParseType
getParseType filePath =
  case fmap reverse $ splitOn "." (reverse filePath) of
    maybeMustacheExtension:maybeFileExtension:_ ->
      let parseSuffix = maybeFileExtension ++ "." ++ maybeMustacheExtension
      in case Map.lookup parseSuffix suffixParseMap of
           Just parseType -> parseType
           Nothing -> Skip
    _ -> Skip


-- | A relative path to a file to parse (excluding its parent-most directory)
-- and which parser shall be used to parse it.
type FileToParse = (FilePath, ParseType)


-- | Creates a list of files to parse in a given directory, along with which parser to use.
filesToParse :: FilePath -> IO [FileToParse]
filesToParse sourceDirectory = do
  filesMatching <- getDirectoryFiles sourceDirectory ["**/*.txt.mustache", "**/*.md.mustache"]
  pure $ fmap (\x -> (x, getParseType x)) filesMatching


-- | Global variables which can be access by a Mustache file being parsed.
--
-- These are the "substitutions" for Mustache. These are functions (lambdas),
-- and regular string values.
--
-- These substitutions are later modified to include a Mustache partial for
-- the Burrow template/partial system.
dataForMustache :: [(Text.Text, Mtype.Value)]
dataForMustache =
  [ ("title", Mtype.String "My Gopherhole")
  , ("justify2", overText justify2)
  , ("justify", overText justify')
  , ("columnate2", overText columnate2)
  ]


-- | Match the "somepartial" of */*/*.somepartial.partial.*.mustache
--
-- Will return Nothing if doesn't use a partial or if the extension isn't
-- mapped to a Markdown parser.
--
-- >>> matchPartial "foo/bar/afile.phlogpost.partial.txt.mustache"
-- Just ("phlogpost", ".txt.mustache")
matchPartial :: FilePath -> Maybe (String, String)
matchPartial filePath =
  case getParseType filePath of
    Skip -> Nothing
    _ ->
      let filename = takeFileName filePath
          dotSplit = reverse $ splitOn "." filename
      in case dotSplit of
           "mustache":fileType:"partial":partialName:_ -> Just (partialName, "." ++ fileType ++ ".mustache")
           _ -> Nothing


-- FIXME: this is confusing
-- FIXME: need to have the .mustache extensions removed from .txt and the .md.mustache extensions
-- removed from menus--maybe they should just be .gophermenu if the name is "index.md.mustache?" and
-- the --spacecookie flag is passed?
-- should be text out... maybe don't even use IO out because you should just pass in contents
-- and file type? idk...
-- NOTE: data for mustache is merely a series of substitutions/globals for a mustache template. please
-- see the dataForMustache variable.
-- | Parse a file, mostly creating a ParseFileRecipe and sending it off to the function (`writeOutBasedOn`)
-- that actually uses the recipe to parse the supplied file.
parseFile :: FilePath -> FilePath -> Bool -> FileToParse -> IO ()
parseFile sourceDirectory destinationDirectory spaceCookie (filePath, parseType) = do
  recipe <- case matchPartial filePath of
              Just (templateToUse, extension) -> recipeWithPartial templateToUse extension
              Nothing -> pure recipeWithoutPartial
  writeOutBasedOn recipe
 where
  filePathIncludingSourceDirectory :: String
  filePathIncludingSourceDirectory = sourceDirectory ++ filePath

  -- Create the recipe for a file that uses partials.
  recipeWithPartial :: String -> String -> IO ParseFileRecipe
  recipeWithPartial templateToUse extension = do
    let partial'sTemplatePath = "templates/" ++ templateToUse ++ extension
    partial <- readFile filePathIncludingSourceDirectory
    -- FIXME, TODO: it feels like this is being done twice!
    let newDataForMustache = ("partial", Mtype.String (pack partial)):dataForMustache
    pure $ ParseFileRecipe
             { pfrSourceDirectory = sourceDirectory
             , pfrDestinationDirectory = destinationDirectory
             , pfrSpacecookie = spaceCookie
             , pfrTemplateToRender = partial'sTemplatePath
             , pfrParseType = parseType
             , pfrOutPath = filePath
             , pfrIncludePartial = Just filePathIncludingSourceDirectory
             , pfrSubstitutions = newDataForMustache
             }

  -- Create the recipe for a file that does not use partials.
  recipeWithoutPartial :: ParseFileRecipe
  recipeWithoutPartial = 
    ParseFileRecipe
      { pfrSourceDirectory = sourceDirectory
      , pfrDestinationDirectory = destinationDirectory
      , pfrSpacecookie = spaceCookie
      , pfrTemplateToRender = filePathIncludingSourceDirectory
      , pfrParseType = parseType
      , pfrOutPath = filePath
      , pfrIncludePartial = Nothing
      , pfrSubstitutions = dataForMustache
      }


-- FIXME: what is this even?!
getCompiledTemplate searchSpace templateToRenderPath = do
  compiled <- automaticCompile searchSpace templateToRenderPath
  case compiled of
    Left err -> error $ show err
    Right template -> pure template


-- | All the settings for a function (`writeOutBasedOn`) to parse a file
-- using Commonmark and Mustache.
data ParseFileRecipe = ParseFileRecipe
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
  , pfrSubstitutions :: [(Text.Text, Mtype.Value)]
  -- ^ The substitutions to be used when parsing the mustache template. You can think of these
  -- as globals or putting things into scope. But it all needs to be a commonmark value: a
  -- string, a function, a partial.
  }


-- TODO: prepareTemplate = do
-- | Prepares the file which needs to be parsed as a Mustache template.
parseMustache :: ParseFileRecipe -> IO Text.Text
parseMustache recipe = do
  secondaryTemplate <- case pfrIncludePartial recipe of
                         Just contentToParsePath -> getCompiledTemplate searchSpace contentToParsePath
                         Nothing -> getCompiledTemplate searchSpace (pfrTemplateToRender recipe)
  -- This is the template we'll be working with!
  mainTemplate <- case pfrIncludePartial recipe of
                    Just _ -> do
                      -- This is where the weirdness comes in. secondaryTemplate will actually be the main
                      -- template with the actual file we want to parse as the *actual* Mustache partial,
                      -- which we put into the secondaryTemplate's namespace/template cache variables/list
                      -- of partials simply as "partial" which is called in the actual template file in
                      -- templates/.
                     -- FIXME: wait considering below is inserting secondarytemplate as the partial is this all wrong :ooo
                      -- FIXME: what the hell is the below line doing?!
                      let templateCache = H.insert "partial" secondaryTemplate (partials secondaryTemplate)
                      compiled' <- compileTemplateWithCache searchSpace templateCache (pfrTemplateToRender recipe)
                      case compiled' of
                        Left err -> error $ show err
                        Right template -> pure template
                    Nothing -> pure secondaryTemplate

  -- could put this in the prepareTemplate?
      -- should put this stuff in Mustache
  -- FIXME: what's happening here?!
  let k = Map.fromList (pfrSubstitutions recipe) :: Map.Map Text.Text Mtype.Value
      -- where is this substitute function coming from?! is it commonmark to preform the partial substitution?
      testContents = substitute mainTemplate k

  pure testContents


-- FIXME: this needs to be broken down into smaller parts.
-- FIXME: what is this mess?!
-- | Parse/write out a file according to the supplied ParseFileRecipe.
writeOutBasedOn :: ParseFileRecipe -> IO ()
writeOutBasedOn recipe = do
  testContents <- parseMustache recipe
  parseMarkdown recipe testContents


-- FIXME: this does too much, including writing out. should just give back bytes or something idk
parseMarkdown :: ParseFileRecipe -> Text.Text -> IO ()
parseMarkdown recipe contents =
  case pfrParseType recipe of
    -- NOTE: The way this type is setup seems redundant/adds extra maintenence just use a sumtype like ParseType = GopherFile | GopherMenu? FIXME/NOTE/TODO
    -- Parse the markdown text out as a text file to be served in gopherspace.
    GopherFileType -> parseOutGopherFile
    -- Parse the markdown text out as a gophermap/menu.
    GopherMenuType -> parseOutGopherMenu
    -- Do not parse this file. Simply copy the file to the new destination!
    Skip -> noParseOut
 where
  -- This is where the file will be writen out to.
  filePath :: FilePath
  filePath = pfrOutPath recipe

  -- When used needs to specify the type. 
  parseCommonmark testContents' = commonmarkWith defaultSyntaxSpec "test" testContents'

  -- Write text to the target/built directory, creating directories in the process if needed.
  -- Will also write out to the file name .gophermap if the input file name matched spacecookieGophermapName.
  writeOut :: Text.Text -> IO ()
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
        let inlineOverrides = InlineOverrides { overrideLink = Just link' }
        let env = Environment { envFonts = allTheAsciiFonts, envInlineOverrides = inlineOverrides }
        let (GopherFile out') = runReader penv env
        writeOut out'
    {-
    case out of
      Left parseError -> error $ show parseError
      Right penv -> do
        allTheAsciiFonts <- getAsciiFonts
        let out' = runReader penv allTheAsciiFonts
            out'' = gopherMenuToText out'
        writeOut out''
    -}

  -- Don't parse; just copy the file to the target directory.
  noParseOut :: IO ()
  noParseOut = do
    let destination = pfrDestinationDirectory recipe ++ filePath
        destinationDirectory = takeDirectory destination
        source = pfrSourceDirectory recipe ++ filePath
    createDirectoryIfMissing True destinationDirectory
    copyFile source destination


-- | The exposed function to parse and copy a directory's files out to a new directory which
-- can be served via the Gopher protocol.
doTheParsing :: FilePath -> FilePath -> Bool -> IO ()
doTheParsing sourceDir destDir spaceCookie =
  filesToParse sourceDir >>= traverse_ (parseFile sourceDir destDir spaceCookie)
