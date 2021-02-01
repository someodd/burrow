-- | Create fancy ASCII-art-type text from an input string.
--
-- Burrow has support for its own font specification, which is used
-- just for creating ASCII-art headings.

module TextUtils.Headings (AsciiFont, headingCompose, getAsciiFonts) where

import Data.Char
import Data.List (transpose, intercalate)
import Data.Maybe (maybe)

import Data.List.Split (splitOn)
import qualified Data.Map as Map


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


-- | Currently a hardcoded reading-in of AsciiFont files, mapping to
-- their heading level name.
--
-- This will be updated in the future to set the various heading levels to specific
-- font locations via a config.
getAsciiFonts :: IO (Map.Map String AsciiFont)
getAsciiFonts = do
  h1 <- parseFont "data/fonts/basicthorpe.bmf"
  -- TODO: in the future I think this should be the heading level as an integer.
  pure $ Map.fromList [("h1", h1)]


-- this is hacks in case... update when ini later
fontLookup :: Char -> AsciiFont -> Maybe [String]
fontLookup c f =
  case Map.lookup (toLower c) f of
    Nothing ->
      case Map.lookup (toUpper c) f of
        Nothing -> Nothing
        x -> x
    x -> x


fontWidthHeight :: AsciiFont -> (Int, Int)
fontWidthHeight font =
  -- temporary hack FIXME (see notes about ini)
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


headingCompose :: AsciiFont -> String -> String
headingCompose font string = "\n" ++ composeJoin
 where
  (_, height) = fontWidthHeight font

  fontLetter font c =
    case fontLookup c font of
      Just a -> a
      Nothing -> replicate height ""

  convert = map (fontLetter font) string

  --addSpace = map (fmap (++ " "))
  --composeJoin = unlines $ map (intercalate "") $ transpose (addSpace convert)
  composeJoin = unlines $ map (intercalate "") $ transpose convert
