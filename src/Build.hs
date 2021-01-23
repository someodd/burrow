-- | The main file in the library. This is where all the code is used to
-- actually parse file(s) to a gopherhole!
{-# LANGUAGE OverloadedStrings          #-}
module Build where

import Data.Maybe (fromJust)
import qualified Data.List as L
import Data.HashMap.Strict as H
import           Data.Text (unpack, pack, Text, toUpper)
import Data.List (isSuffixOf)
import Data.Foldable (traverse_)
import System.FilePath (takeDirectory, takeFileName)

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


-- | This indicates if a file should be parsed as a text file intended
-- for gopherspace, or if it should be parsed as a gophermap/menu.
data ParseType = GopherFileType | GopherMenuType

-- | A file to parse must have its preserved path (excluding its base/source
-- directory, but all the directories after that) and it's ParseType so that
-- it can replicate the directory structure to the new path and render the
-- files according to its ParseType.
type FileToParse = (FilePath, ParseType)


-- | This loads a bunch of files from disk into the environment map for...

-- FIXME: should error otherwise
-- | Assumes you're providing *.md.mustache or *.txt.mustache.
determineType :: String -> ParseType
determineType filename =
  if ".txt.mustache" `isSuffixOf` filename
    then GopherFileType
    else if ".md.mustache" `isSuffixOf` filename
      then GopherMenuType
      else error "Filename must end in either .txt.mustache or .md.mustache!"


filesToParse :: FilePath -> IO [FileToParse]
filesToParse sourceDirectory = do
  filesMatching <- getDirectoryFiles sourceDirectory ["**/*.txt.mustache", "**/*.md.mustache"]
  pure $ fmap (\x -> (x, determineType x)) filesMatching


dataForMustache = [("title", Mtype.String "My Gopherhole"), ("justify2", overText justify2), ("justify", overText justify'), ("columnate2", overText columnate2)]

-- FIXME: need to have the .mustache extensions removed from .txt and the .md.mustache extensions
-- removed from menus--maybe they should just be .gophermenu if the name is "index.md.mustache?" and
-- the --spacecookie flag is passed?
-- should be text out... maybe don't even use IO out because you should just pass in contents
-- and file type? idk...
parseFile :: FilePath -> FilePath -> Bool -> FileToParse -> IO ()
parseFile sourceDirectory destinationDirectory spaceCookie (filePath, parseType) = do
  let filePathIncludingSourceDirectory = sourceDirectory ++ filePath

  case matchPartial filePath of
    Just (templateToUse, extension) -> do
      let partial'sTemplatePath = "templates/" ++ templateToUse ++ extension
      partial <- readFile filePathIncludingSourceDirectory
      let newDataForMustache = ("partial", Mtype.String (pack partial)):dataForMustache
      writeOutBasedOn destinationDirectory spaceCookie partial'sTemplatePath newDataForMustache parseType filePath (Just filePathIncludingSourceDirectory)
    Nothing ->
      ---do the normal thing
      writeOutBasedOn destinationDirectory spaceCookie filePathIncludingSourceDirectory dataForMustache parseType filePath Nothing
 where
  -- | Match the 'somepartial" of somefilename.somepartial.partial.*.mustache
  matchPartial :: FilePath -> Maybe (String, String)
  matchPartial filePath = do
    let filename = takeFileName filePath
        dotSplit = reverse $ splitOn "." filename
    case dotSplit of
      "mustache":"txt":"partial":partialName:_ -> Just (partialName, ".txt.mustache")
      "mustache":"md":"partial":partialName:_ -> Just (partialName, ".md.mustache")
      _ -> Nothing


getCompiledTemplate searchSpace templateToRenderPath = do
  compiled <- automaticCompile searchSpace templateToRenderPath
  case compiled of
    Left err -> error $ show err
    Right template -> pure template


writeOutBasedOn destinationDirectory spaceCookie templateToRenderPath dataForMustache parseType outPath addPartial = do
  secondaryTemplate <- case addPartial of
                         Just contentToParsePath -> getCompiledTemplate searchSpace contentToParsePath
                         Nothing -> getCompiledTemplate searchSpace templateToRenderPath
  mainTemplate <- case addPartial of
                    Just _ -> do
                      -- parse the main template... 
                      --t <- getCompiledTemplate searchSpace templateToRenderPath
                      let templateCache = H.insert "partial" secondaryTemplate (partials secondaryTemplate)
                      compiled' <- compileTemplateWithCache searchSpace templateCache templateToRenderPath
                      case compiled' of
                        Left err -> error $ show err
                        Right template -> pure template
                    Nothing -> pure secondaryTemplate

  -- shouldn't the template be decided based off its type i guess? or shoudl we leave it
  -- up to user
  -- tell if partial by *.something.partial.something.mustache
  -- this will indicate if should use the "something" partial
  let filePath = outPath

      -- should put this stuff in Mustache
  let k = Map.fromList dataForMustache :: Map.Map Text.Text Mtype.Value
      testContents = substitute mainTemplate k

  -- FIXME: shouldn't there be a better way of doing this?
  -- Parse markdown
  case parseType of
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
 where
  outParse testContents' = commonmarkWith defaultSyntaxSpec "test" testContents'
  -- needs to create directories too FIXME/TODO
  outCheck out' filePath = do
    let outPath =
          if spaceCookie && spacecookieGophermapName `isSuffixOf` filePath
            then let x = (takeDirectory $ destinationDirectory ++ filePath) in x ++ "/.gophermap"
            else destinationDirectory ++ filePath
        directory = takeDirectory outPath
    createDirectoryIfMissing True directory
    case out' of
      Left parseError -> error $ show parseError
      Right gopher -> writeFile outPath (show gopher)

  outCheck' out' filePath = do
    let outPath =
          if spaceCookie && spacecookieGophermapName `isSuffixOf` filePath
            then let x = (takeDirectory $ destinationDirectory ++ filePath) in x ++ "/.gophermap"
            else destinationDirectory ++ filePath
        directory = takeDirectory outPath
    createDirectoryIfMissing True directory
    writeFile outPath (T.unpack out')



-- | Writes out to equivalent...
--writeFileOut

-- FIXME: use filepaths to ensure trailing slash
doTheParsing :: FilePath -> FilePath -> Bool -> IO ()
doTheParsing sourceDir destDir spaceCookie =
  filesToParse sourceDir >>= traverse_ (parseFile sourceDir destDir spaceCookie)

-- | This is the special name for generating spacecookie indexes if enabled (gophermaps)
spacecookieGophermapName = "index.md.mustache"


