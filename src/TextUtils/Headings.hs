-- | Create fancy ASCII-art-type text from an input string.
--
-- Burrow has support for its own font specification, which is used
-- just for creating ASCII-art headings. See the README for more
-- details on the font specification and configuring Burrow to use
-- different fonts.

module TextUtils.Headings
  ( HeadingLevelFontMap
  , AsciiFont
  , headingCompose
  , getAsciiFonts
  )
where

import Data.Char
import Data.List (transpose, intercalate)
import Data.Maybe (maybe)

import Data.List.Split (splitOn)
import qualified Data.Map as Map

import Config


-- | An ASCII art character is represented by breaking up each line into an element of a list.
type AsciiChar = [String]

-- | An ASCII font is represented this way to make it easy to lookup a regular character and
-- get the AsciiChar in return.
type AsciiFont = Map.Map Char AsciiChar


-- | Parse a Burrow ASCII art font file.
--
-- This spec is subject to change. Currently only supports monospaced fonts.
--
-- See the project's README for the font spec.
parseFont :: FilePath -> IO AsciiFont
parseFont path = do
  fontFileContents <- readFile path
  pure $ Map.fromList $ map charLegendsToPair (splitIntoCharLegends fontFileContents)
 where
  splitIntoCharLegends = splitOn "\n\n"
  charLegendsToPair charLegend =
    case lines charLegend of
      x:[] -> error "Char legend misformatted."
      x:xs ->
        case x of
          c:[] -> (c, xs)
          _ -> error "Legend in char legend is not just a single character."
      _ -> error "Char legend misformatted."


-- | Specifies which AsciiFont is related to which heading level.
type HeadingLevelFontMap = Map.Map Int AsciiFont


-- | Get all the fonts loaded, mapped to a specific heading level,
-- according to the configuration file.
getAsciiFonts :: IO HeadingLevelFontMap
getAsciiFonts = do
  -- FIXME: this is all dreadful! do this automatically
  -- maybe via recursion?
  configParser <- getConfig
  h1Location <- getConfigValue configParser "fonts" "1"
  h2Location <- getConfigValue configParser "fonts" "2"
  h3Location <- getConfigValue configParser "fonts" "3"
  h4Location <- getConfigValue configParser "fonts" "4"
  h5Location <- getConfigValue configParser "fonts" "5"
  h6Location <- getConfigValue configParser "fonts" "6"

  h1 <- parseFont h1Location
  h2 <- parseFont h2Location
  h3 <- parseFont h3Location
  h4 <- parseFont h4Location
  h5 <- parseFont h5Location
  h6 <- parseFont h6Location

  -- TODO: in the future I think this should be the heading level as an integer.
  pure $ Map.fromList
    [ (1, h1)
    , (2, h2)
    , (3, h3)
    , (4, h4)
    , (5, h5)
    , (6, h6)
    ]


-- | Look up the ascii art character which corresponds to the supplied character.
--
-- Hacky and lazy. Will update when specify flags later (like allUpper, allLower) in the spec.
fontLookup :: Char -> AsciiFont -> Maybe [String]
fontLookup c f =
  case Map.lookup (toLower c) f of
    Nothing ->
      case Map.lookup (toUpper c) f of
        Nothing -> Nothing
        x -> x
    x -> x


-- | For monospaced fonts (all Burrow supports at the moment), get the
-- width and height of each ascii art character based on the lowercase
-- a, preferably first, otherwise use the uppercase 'A'. These ASCII
-- art characters must not have blank first lines.
--
-- A newer spec will let you specify which character defines the font
-- width and height if the monospaced font flag is set.
fontWidthHeight :: AsciiFont -> (Int, Int)
fontWidthHeight font =
  case Map.lookup 'a' font of
    Nothing ->
      case Map.lookup 'A' font of
        Nothing -> error "Font width/height error: neither 'a' nor 'A' defined."
        Just a -> determineWidthHeightFromA a
    Just a -> determineWidthHeightFromA a
 where
  determineWidthHeightFromA a =
    let
      firstLineLength = length (head a)
    in
      if firstLineLength < 1
        then error "First line length <0"
        else (length $ head a, length a)


-- | The main function: ...
headingCompose :: AsciiFont -> String -> String
headingCompose font string = "\n" ++ composeJoin
 where
  (_, height) = fontWidthHeight font

  fontLetter font c =
    case fontLookup c font of
      Just a -> a
      Nothing -> replicate height ""

  convert = map (fontLetter font) string

  composeJoin = unlines $ map (intercalate "") $ transpose convert
