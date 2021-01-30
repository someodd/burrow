-- FIXME: make note that all parseable files must be some kind of *.mustache?
-- | The main file in the library. This is where all the code is used to
-- actually parse file(s) to a gopherhole!
{-# LANGUAGE OverloadedStrings          #-}
module Build 
  ( doTheParsing
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
import Markdown.ToTextFile
import Markdown.ToGopherMenu
import Mustache
import Types


-- | This is the special name for generating spacecookie indexes if enabled (gophermaps)
spacecookieGophermapName = "index.md.mustache"


-- NOTE: is there something like this in System.FilePath?
type FileName = FilePath


-- | The parser to use to transform Markdown files to various outputs
-- (like menus/gophermaps) suitable for gopherspace.
data ParseType = GopherFileType
               -- ^ Parse to plaintext ASCII-art-style file.
               | GopherMenuType
               -- ^ Parse to a gophermap/Gopher menu.
               -- NOTE: for below: maybe better named "CopyOnly!"
               | Skip
               -- ^ Will not be parsed! Will simply be copied. Maybe better named 


-- TODO: create a map of file extension to parser type?
type ParseSuffix = String


-- | The supported file extensions/file types which are mapped
-- to a specific parser.
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


-- | Creates a list of files to parse, along with which parser to use.
filesToParse :: FilePath -> IO [FileToParse]
filesToParse sourceDirectory = do
  filesMatching <- getDirectoryFiles sourceDirectory ["**/*.txt.mustache", "**/*.md.mustache"]
  pure $ fmap (\x -> (x, getParseType x)) filesMatching


-- | Global variables which can be access by a Mustache file being parsed.
dataForMustache = [("title", Mtype.String "My Gopherhole"), ("justify2", overText justify2), ("justify", overText justify'), ("columnate2", overText columnate2)]


-- | Match the 'somepartial" of foo/bar/somefilename.somepartial.partial.*.mustache
--
-- Will return Nothing if doesn't use a partial or isn't a valid parseable file.
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
parseFile :: FilePath -> FilePath -> Bool -> FileToParse -> IO ()
parseFile sourceDirectory destinationDirectory spaceCookie (filePath, parseType) = do
  case matchPartial filePath of
    Just (templateToUse, extension) -> parseUsingPartial templateToUse extension
    Nothing -> parseWithoutPartial
 where
  filePathIncludingSourceDirectory = sourceDirectory ++ filePath
  parseUsingPartial templateToUse extension = do
    let partial'sTemplatePath = "templates/" ++ templateToUse ++ extension
    partial <- readFile filePathIncludingSourceDirectory
    -- FIXME, TODO: it feels like this is being done twice!
    let newDataForMustache = ("partial", Mtype.String (pack partial)):dataForMustache
    let recipe = ParseFileRecipe
                   { pfrSourceDirectory = sourceDirectory
                   , pfrDestinationDirectory = destinationDirectory
                   , pfrSpacecookie = spaceCookie
                   , pfrTemplateToRender = partial'sTemplatePath
                   , pfrParseType = parseType
                   , pfrOutPath = filePath
                   , pfrIncludePartial = Just filePathIncludingSourceDirectory
                   , pfrSubstitutions = newDataForMustache
                   }
    writeOutBasedOn recipe

  parseWithoutPartial = 
    ---do the normal thing
    let recipe = ParseFileRecipe
                   { pfrSourceDirectory = sourceDirectory
                   , pfrDestinationDirectory = destinationDirectory
                   , pfrSpacecookie = spaceCookie
                   , pfrTemplateToRender = filePathIncludingSourceDirectory
                   , pfrParseType = parseType
                   , pfrOutPath = filePath
                   , pfrIncludePartial = Nothing
                   , pfrSubstitutions = dataForMustache
                   }
    in writeOutBasedOn recipe


getCompiledTemplate searchSpace templateToRenderPath = do
  compiled <- automaticCompile searchSpace templateToRenderPath
  case compiled of
    Left err -> error $ show err
    Right template -> pure template


-- | ...
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

-- FIXME: what is this mess?!
--writeOutBasedOn sourceDirectory destinationDirectory spaceCookie templateToRenderPath dataForMustache parseType outPath addPartial = do
writeOutBasedOn recipe = do
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


  -- shouldn't the template be decided based off its type i guess? or shoudl we leave it
  -- up to user
  -- tell if partial by *.something.partial.something.mustache
  -- this will indicate if should use the "something" partial
  let filePath = (pfrOutPath recipe)

      -- should put this stuff in Mustache
  -- FIXME: what's happening here?!
  let k = Map.fromList (pfrSubstitutions recipe) :: Map.Map Text.Text Mtype.Value
      -- where is this substitute function coming from?! is it commonmark to preform the partial substitution?
      testContents = substitute mainTemplate k

  -- FIXME: shouldn't there be a better way of doing this?
  -- Parse markdown
  case pfrParseType recipe of
    -- The way this type is setup seems redundant/adds extra maintenence just use a sumtype like ParseType = GopherFile | GopherMenu? FIXME/NOTE/TODO
    GopherFileType -> do
      out <- outParse testContents :: IO (Either ParseError (ParseEnv GopherFile))
      case out of
        Left parseError -> error $ show parseError
        Right penv -> do
          allTheAsciiFonts <- getAsciiFonts
          let (GopherFile out') = runReader penv allTheAsciiFonts
          outCheck' out' filePath
    GopherMenuType -> do
      out <- outParse testContents :: IO (Either ParseError (ParseEnv GopherMenu))
      case out of
        Left parseError -> error $ show parseError
        Right penv -> do
          allTheAsciiFonts <- getAsciiFonts
          let out' = runReader penv allTheAsciiFonts
              out'' = gopherMenuToText out'
          outCheck' out'' filePath
    -- Simply copy the file to the new destination!
    Skip -> do
      let destination = pfrDestinationDirectory recipe ++ filePath
          destinationDirectory = takeDirectory destination
          source = pfrSourceDirectory recipe ++ filePath
      createDirectoryIfMissing True destinationDirectory
      copyFile source destination
 where
  outParse testContents' = commonmarkWith defaultSyntaxSpec "test" testContents'

  outCheck' out' filePath = do
    let outPath =
          if (pfrSpacecookie recipe) && spacecookieGophermapName `isSuffixOf` filePath
            then let x = (takeDirectory $ pfrDestinationDirectory recipe ++ filePath) in x ++ "/.gophermap"
            else pfrDestinationDirectory recipe ++ filePath
        directory = takeDirectory $ pfrDestinationDirectory recipe ++ (pfrOutPath recipe)
    createDirectoryIfMissing True directory
    writeFile outPath (T.unpack out')


-- | Writes out to equivalent...
--writeFileOut

-- FIXME: use filepaths to ensure trailing slash
doTheParsing :: FilePath -> FilePath -> Bool -> IO ()
doTheParsing sourceDir destDir spaceCookie =
  filesToParse sourceDir >>= traverse_ (parseFile sourceDir destDir spaceCookie)
